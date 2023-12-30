package com.logicaldoc.core.store;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;

import com.logicaldoc.core.document.Document;

/**
 * The Storer manages the repository where document files are maintained and all
 * general resources are stored.
 * 
 * @author Michael Scholz, Marco Meschieri
 */
public interface Storer extends Comparable<Storer> {

	/**
	 * The unique identifier
	 * 
	 * @return the storer identifier
	 */
	public int getId();

	/**
	 * Sets the unique identifier
	 * 
	 * @param id the storer identifier
	 */
	public void setId(int id);

	/**
	 * This method has to store a resource in the document's container. The
	 * location where (DBMS, Filesystem, other) the document should be stored is
	 * defined by the concrete implementation. It is possible to store a new
	 * document or a new version of an existing document.
	 * 
	 * @param stream Document as InputStream
	 * @param docId The document identifier
	 * @param resource Name of the resource to be stored
	 * 
	 * @throws IOException the content cannot be stored
	 */
	public void store(InputStream stream, long docId, String resource) throws IOException;

	/**
	 * Stores a file
	 * 
	 * @see store(InputStream stream, long docId, String resource)
	 * 
	 * @param file the file to store
	 * @param docId identifier of the document
	 * @param resource name of the resource
	 * 
	 * @throws IOException the content cannot be stored
	 */
	public void store(File file, long docId, String resource) throws IOException;

	/**
	 * Deletes all resources of a document from the storage.
	 * 
	 * @param docId The document identifier
	 */
	public void delete(long docId);

	/**
	 * Deletes a specific resource of a document from the storage.
	 * 
	 * @param docId The document identifier
	 * @param resource Name of the resource to be deleted
	 */
	public void delete(long docId, String resource);

	/**
	 * Computes the resource name inside the container
	 * 
	 * @param doc The document representation
	 * @param fileVersion The file version (use null for the latest version)
	 * @param suffix The file suffix (use null if you want the exact document
	 *        file)
	 * @return The document's resource name
	 */
	public String getResourceName(Document doc, String fileVersion, String suffix);

	/**
	 * Computes the resource name inside the container
	 * 
	 * @param docId The document identifier
	 * @param fileVersion The file version (use null for the latest version)
	 * @param suffix The file suffix (use null if you want the exact document
	 *        file)
	 * @return The document's resource name
	 */
	public String getResourceName(long docId, String fileVersion, String suffix);

	/**
	 * Lists all resources in the document's container
	 * 
	 * @param docId The document's identifier
	 * @param fileVersion If specified, lists the resources for that specific
	 *        file version only
	 * 
	 * @return list of resource names
	 */
	public List<String> listResources(long docId, String fileVersion);

	/**
	 * Computed the size of a specific resource.
	 * 
	 * @param docId The document's identifier
	 * @param resource The resource
	 * 
	 * @return the size in bytes
	 */
	public long size(long docId, String resource);

	/**
	 * Checks if the passed resource exists in the document's container
	 * 
	 * @param docId ID of the document
	 * @param resource Name of the resource
	 * @return true only if the resource already exists
	 */
	public boolean exists(long docId, String resource);

	/**
	 * Writes the specified resource in a file
	 * 
	 * @param docId The document identifier
	 * @param resource Name of the resource
	 * @param out File that will receive the resource's content
	 * 
	 * @throws IOException error writing the file or reading the resource
	 */
	public void writeToFile(long docId, String resource, File out) throws IOException;

	/**
	 * Writes the specified resource in an output stream
	 * 
	 * @param docId The document's identifier
	 * @param resource Name of the resource
	 * @param output The output stream
	 * @param start Index of the starting byte
	 * @param length Total packet length
	 * 
	 * @throws IOException error writing the stream or reading the resource
	 */
	public void writeToStream(long docId, String resource, OutputStream output, long start, long length)
			throws IOException;

	/**
	 * Writes the specified resource in an output stream
	 * 
	 * @param docId The document's identifier
	 * @param resource Name of the resource
	 * @param output The output stream
	 * 
	 * @throws IOException error writing the stream or reading the resource
	 */
	public void writeToStream(long docId, String resource, OutputStream output) throws IOException;

	/**
	 * Obtains the document's content for the specified resource
	 * 
	 * @param docId The document's identifier
	 * @param resource Name of the resource
	 * 
	 * @return The document file's content
	 * 
	 * @throws IOException cannot open the stream
	 */
	public InputStream getStream(long docId, String resource) throws IOException;

	/**
	 * Obtains the document's raw bytes for the specified resource
	 * 
	 * @param docId The document's identifier
	 * @param resource Name of the resource
	 * 
	 * @return The document file's bytes
	 * 
	 * @throws IOException cannot open the resource to get the bytes
	 */
	public byte[] getBytes(long docId, String resource) throws IOException;

	/**
	 * Obtains the document's raw bytes for the specified resource
	 * 
	 * @param docId The document's identifier
	 * @param resource Name of the resource
	 * @param start Index of the starting byte
	 * @param length Total packet length
	 * 
	 * @return The document file's bytes
	 * 
	 * @throws IOException cannot open the resource to get the bytes
	 */
	public byte[] getBytes(long docId, String resource, long start, long length) throws IOException;

	/**
	 * Moves all the resources of a document from it's original location to a
	 * different storage
	 * 
	 * @param docId identifier of the document to process
	 * @param targetStorageId identifier of the storage that will receive the
	 *        files
	 * 
	 * @return number of moved resources
	 * 
	 * @throws IOException In case of error during the process
	 */
	public int moveResourcesToStore(long docId, int targetStorageId) throws IOException;

	/**
	 * Obtains the document's content as string for the specified resource
	 * 
	 * @param docId The document's identifier
	 * @param resource Name of the resource
	 * 
	 * @return The document file's as string representation
	 */
	public String getString(long docId, String resource);

	/**
	 * Computes the total size of the documents repository(in bytes)
	 * 
	 * @return sum of the sizes of all the documents expressed in bytes
	 */
	public long getTotalSize();

	/**
	 * Implementations should return the list of the required parameters. A
	 * parameter is stored in the context as storer.<b>id</b>.parameter = value
	 * 
	 * @return list of parameter names
	 */
	public List<String> getParameterNames();

	/**
	 * Returns the map of parameters
	 * 
	 * @return a map with settings <b>setting_name</b> - <b>setting_value</b>
	 */
	public Map<String, String> getParameters();

	/**
	 * Tests if the storer can read and write
	 * 
	 * @return if the storer can read and write
	 */
	public boolean test();

	/**
	 * Tests if the storer is enabled
	 * 
	 * @return if the storer is enabled
	 */
	public boolean isEnabled();

	/**
	 * Initialization method
	 */
	public void init();

	/**
	 * Destroy method
	 */
	public void destroy();

	/**
	 * Instantiate a new storer and fully configures it.
	 * 
	 * @param id identifier of the storer to create
	 * 
	 * @return the created instance
	 */
	public Storer newStorer(int id);
	
	/**
	 * Retrieves the storers definitions grouped by type 
	 * 
	 * @return a map with definitions  <b>type</b> - <b>storer prototype</b>
	 */
	public Map<String, Storer> getStorerDefinitions();
}