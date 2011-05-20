
package filebox

import spec._

import com.amazonaws.auth._
import com.amazonaws.services.s3._
import com.amazonaws.services.s3.model._

import java.io.{
    ByteArrayInputStream,
    ByteArrayOutputStream,
    InputStream,
    OutputStream
}

import java.security._
import javax.crypto.spec._

import scala.collection.JavaConversions._
import scala.collection.mutable.{Set => MutableSet}


/**
 * An implementation of the block storage that uses Amazon's S3 service to
 * store data. This implementation uses the encrypting s3 client that does
 * AES/CBC/PKCS5Padding encryption of all data passed to the service. The
 * encryption key supplied by the user is hashed first using SHA-256, the hash
 * then is used as the encryption key for the s3 client.
 *
 * This storage implementation lazily fetches all block names from the s3
 * service and caches them to make sure that the duplicate blocks are not sent
 * to the sotrage. This is done because AWS is happy to overwrite files and
 * does not report that the file exists without explicit checks. This approach
 * might not work well for huge datastores as it would place big demand on
 * memory and so it might be revised in the future.
 *
 * Note, that the block signature is computer before encryption and so if the
 * encryption key is changed between backups some of the blocks will be
 * encrypted with one key and some with another making the restore operation
 * impossible.
 * TODO rethink the encyption scheme.
 */
class S3BlockStorage(
    bucketName: String,
    prefix: String,
    accessKey: String,
    secretKey: String,
    encryptionKey: String) extends BlockStorage
{

    private val key = new SecretKeySpec(hashedEncryptionKey, "AES")

    private val awsCredential = new BasicAWSCredentials(accessKey, secretKey)
    private val encMaterials = new EncryptionMaterials(key)
    private val s3client = new AmazonS3EncryptionClient(
        awsCredential, encMaterials)

    // A cache of all block file names in the storage (sig + dataType).
    lazy val blockCache: MutableSet[String] = fetchBlockNames


    def write(data: Array[Byte], len: Int, dataType: String): Sig = {
        val sig = Sig(data, len)
        val cacheEntry = sig.toString + "." + dataType

        if(blockCache(cacheEntry)) {
            return sig
        }

        val om = new ObjectMetadata
        om.setContentLength(len)

        val is = new ByteArrayInputStream(data, 0, len)
        try {
            val res = s3client.putObject(
                bucketName, blockFileName(sig, dataType), is, om)
        } finally {
            is.close
        }

        blockCache += cacheEntry

        sig
    }


    def read(digest: Sig, dataType: String): Array[Byte] = {
        val name = blockFileName(digest, dataType)
        val o = s3client.getObject(bucketName, name)

        val is = o.getObjectContent
        val os = new ByteArrayOutputStream

        try {
            spool(is, os)
        } finally {
            is.close
        }

        os.toByteArray
    }


    def listMetadata: List[String] = readFiles(metaDirName)


    def writeMetadata(name: String, data: Array[Byte], len: Int) {
        val om = new ObjectMetadata
        om.setContentLength(len)

        // TODO this will always rewrite the object in the store, instead
        // should check if the name exists first!
        val is = new ByteArrayInputStream(data, 0, len)
        try {
            val res = s3client.putObject(
                bucketName,
                metaFileName(name),
                is,
                om)
        } finally {
            is.close
        }
    }


    def readMetadata(name: String): Array[Byte] = {
        val o = s3client.getObject(bucketName, metaFileName(name))

        val is = o.getObjectContent
        val os = new ByteArrayOutputStream

        try {
            spool(is, os)
        } finally {
            is.close
        }

        os.toByteArray
    }


    // Reads and returns a list of files starting with the given prefix.
    private def readFiles(pref: String): List[String] = {
        // Convert the object listing into a list of names removing the prefix
        // in the beginning.
        def objsToNames(objs: ObjectListing): List[String] = {
            val t = objs.getObjectSummaries map {x =>
                x.getKey.substring(pref.size + 1)}
            t.toList
        }

        // Collecting list of list of names for each batch.
        def nextObjects(objs: ObjectListing,
                        acc: List[List[String]]): List[List[String]] =
        {
            val ret = objsToNames(objs) :: acc

            if(objs.isTruncated) {
                nextObjects(s3client.listNextBatchOfObjects(objs), ret)
            } else {
                ret
            }
        }

        nextObjects(s3client.listObjects(bucketName, pref), List()).flatten
    }


    /**
     * Fetches all block names from the storage and constructs a set.
     */
    private def fetchBlockNames: MutableSet[String] =
        MutableSet() ++ readFiles(blockDirName)


    // Just spool the input stream into the output stream.
    private def spool(is: InputStream, os: OutputStream) {
        val buf = new Array[Byte](2048)
        var len = 0

        do {
            len = is.read(buf)
            if(len > 0) os.write(buf, 0, len)
        } while(len > 0)
    }


    private def metaDirName = prefix + "/meta"
    private def blockDirName = prefix + "/block"
    private def blockFileName(sig: Sig, dataType: String): String =
        blockDirName + "/" + sig.toString + "." + dataType
    private def metaFileName(name: String) = metaDirName + "/" + name


    private def hashedEncryptionKey = {
        val hash = MessageDigest.getInstance("SHA-256")
        hash.digest(encryptionKey.getBytes("UTF-8"))
    }
}


object S3BlockStorage {
    def apply(destination: SpecGroup) = {
        val name = destination.stringValue("name").get

        val bucket = destination.stringValue("bucket") getOrElse {
            throw new IllegalArgumentException("Destination " + name +
                " has no bucket")
        }

        val prefix = destination.stringValue("prefix") getOrElse ""

        val accessKey = destination.stringValue("accessKey") getOrElse {
            throw new IllegalArgumentException("Destination " + name +
                " has no access key")
        }

        val secretKey = destination.stringValue("secretKey") getOrElse {
            throw new IllegalArgumentException("Destination " + name +
                " has no secret key")
        }

        val encryptionKey = destination.stringValue("encryptionKey") getOrElse {
            throw new IllegalArgumentException("Destination " + name +
                " has no encryption key")
        }

        new S3BlockStorage(bucket, prefix, accessKey, secretKey, encryptionKey)
    }
}

