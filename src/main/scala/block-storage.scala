
package filebox


/**
 * The trait that represents a block storage. The storage is modeled after
 * plan-9's Venti server. All blocks in the storage are refenced by their
 * digest and type.
 */
// TODO should take input and output streams instead of arrays
trait BlockStorage {
    /**
     * Writes the given block of data to the storage and returns its signature.
     * The signature is returned for convenience only, the caller should be
     * able to compute the signature on its own and later use it to look the
     * data up.
     */
    def write(data: Array[Byte], len: Int, dataType: String): Sig

    /**
     * Returns a block of data referenced by the signature and the data type.
     * TODO should throw if not found
     */
    def read(digest: Sig, dataType: String): Array[Byte]


    /**
     * Returns names of all metadat records in the storage.
     */
    def listMetadata: List[String]


    /**
     * Writes a metadata record with the given name. It is expected that the
     * amount of data passed to this method is small. It is an error to write a
     * record with an existing name.
     */
    def writeMetadata(name: String, data: Array[Byte], len: Int)


    /**
     * Reads and returns a metadata record with the given name.
     */
    def readMetadata(name: String): Array[Byte]
}

