package com.logicaldoc.webservice.soap;

import java.io.IOException;

import javax.activation.DataHandler;
import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;
import javax.mail.MessagingException;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.parser.ParseException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.doc.WSDoc;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSLink;
import com.logicaldoc.webservice.model.WSNote;
import com.logicaldoc.webservice.model.WSRating;
import com.logicaldoc.webservice.model.WSRight;

/**
 * Document Web Service definition interface
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
@WSDoc(description = "documents handling and CRUD operations")
@WebService(name = "Document", serviceName = "Document", targetNamespace = "http://ws.logicaldoc.com")
public interface DocumentService {

	/**
	 * Create a new document. The user can completely customize the document
	 * through a value object containing the document's metadata
	 * 
	 * @param sid identifier of the session
	 * @param document Web service value object containing the document's
	 *        metadata
	 * @param content The document's binary content
	 * 
	 * @return The value object containing the document's metadata
	 * 
	 * @throws IOException I/O error
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod(action = "create")
	@WebResult(name = "document")
	@WSDoc(description = "creates a new document; the user can completely customize the document through a value object containing the document's metadata; returns the newly created document.")
	public WSDocument create(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "document")
	WSDocument document, @WSDoc(description = "the raw content of the file")
	@WebParam(name = "content")
	DataHandler content)
			throws IOException, AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	/**
	 * Deletes an existing document with the given identifier.
	 * 
	 * @param sid identifier of the session
	 * @param docId The document id
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod(action = "delete")
	@WSDoc(description = "deletes an existing document with the given identifier")
	public void delete(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "Document ID")
	@WebParam(name = "docId")
	long docId) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Locks an existing document with the given identifier.
	 * 
	 * @param sid identifier of the session
	 * @param docId The document id
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod(action = "lock")
	@WSDoc(description = "locks an existing document with the given identifier")
	public void lock(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "Document ID", required = true)
	@WebParam(name = "docId")
	long docId) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Unlocks an existing document with the given identifier.
	 * 
	 * @param sid identifier of the session
	 * @param docId identifier of the document
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod(action = "unlock")
	@WSDoc(description = "unlocks an existing document with the given identifier")
	public void unlock(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "identifier of the document", required = true)
	@WebParam(name = "docId")
	long docId) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Renames the title of an existing document with the given identifier.
	 * 
	 * @param sid identifier of the session
	 * @param docId The document id
	 * @param name The new document title
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod(action = "rename")
	@WSDoc(description = "renames the title of an existing document with the given identifier")
	public void rename(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "Document ID", required = true)
	@WebParam(name = "docId")
	long docId, @WebParam(name = "name")
	String name) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Moves an existing document with the given identifier.
	 * 
	 * @param sid identifier of the session
	 * @param docId The document id
	 * @param folderId Identifier of the new document's folder
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod(action = "move")
	@WSDoc(description = "moves an existing document with the given identifier")
	public void move(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "Document ID")
	@WebParam(name = "docId")
	long docId, @WSDoc(description = "identifier of the new document's folder")
	@WebParam(name = "folderId")
	long folderId) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Copies a document into another folder.
	 * 
	 * @param sid identifier of the session
	 * @param docId The document id
	 * @param folderId Identifier of the new document's folder
	 * @param links If links must be copied too
	 * @param notes If notes and annotations must be copied too
	 * 
	 * @return The created copy
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 * @throws IOException I/O error
	 */
	@WebMethod(action = "copy")
	@WebResult(name = "document")
	@WSDoc(description = "copies a document into a folder")
	public WSDocument copy(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "Document ID")
	@WebParam(name = "docId")
	long docId, @WSDoc(description = "identifier of the new document's folder")
	@WebParam(name = "folderId")
	long folderId, @WSDoc(description = "if the links must be copied too")
	@WebParam(name = "links")
	boolean links, @WSDoc(description = "if the notes and annotations must be copied too")
	@WebParam(name = "notes")
	boolean notes, @WSDoc(description = "if the security settings must be copied too")
	@WebParam(name = "security")
	boolean security)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException, IOException;

	/**
	 * Gets the metadata of an existing document with the given identifier
	 * 
	 * @param sid identifier of the session
	 * @param docId The document id
	 * 
	 * @return A value object containing the document's metadata
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod(action = "getDocument")
	@WebResult(name = "document")
	@WSDoc(description = "gets the metadata of an existing document with the given identifier; returns the document's representation")
	public WSDocument getDocument(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "Document ID")
	@WebParam(name = "docId")
	long docId) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Gets document metadata of an existing document with the given custom
	 * identifier
	 * 
	 * @param sid identifier of the session
	 * @param customId The custom id
	 * 
	 * @return A value object containing the document's metadata
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod(action = "getDocumentByCustomId")
	@WebResult(name = "document")
	@WSDoc(description = "gets document metadata of an existing document with the given custom identifier")
	public WSDocument getDocumentByCustomId(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "customId")
	String customId) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Gets document metadata of a collection of existing documents with the
	 * given identifiers
	 * 
	 * @param sid identifier of the session
	 * @param docIds identifiers of the documents
	 * 
	 * @return the list of documents
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "getDocuments")
	@WebResult(name = "documents")
	@WSDoc(description = "gets document metadata of a collection of existing documents with the given identifiers; returns an array of WSDocument")
	public WSDocument[] getDocuments(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "docIds")
	Long[] docIds) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Gets the aliases of the given document
	 * 
	 * @param sid identifier of the session
	 * @param docId The master document ID
	 * @return Arrays of aliases
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "getAliases")
	@WebResult(name = "aliases")
	@WSDoc(description = "gets the aliases of the given document; returns an array of WSDocument that are aliases")
	public WSDocument[] getAliases(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "Document ID")
	@WebParam(name = "docId")
	long docId) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Updates an existing document with the value object containing the
	 * document's metadata
	 * 
	 * @param sid identifier of the session
	 * @param document The value object containing the document's metadata
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod(action = "update")
	@WSDoc(description = "updates an existing document with the value object containing the document's metadata")
	public void update(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "document")
	WSDocument document) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	/**
	 * Gets the content of an existing document with the given identifier
	 * 
	 * @param sid identifier of the session
	 * @param docId The document id
	 * 
	 * @return The requested document's binary
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 * @throws IOException I/O error
	 */
	@WebMethod(action = "getContent")
	@WSDoc(description = "gets the content of an existing document with the given identifier; returns the raw content of the file")
	public DataHandler getContent(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "Document ID")
	@WebParam(name = "docId")
	long docId)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException, IOException;

	/**
	 * Gets the document's text stored in the full-text index
	 * 
	 * @param sid identifier of the session
	 * @param docId The document id
	 * 
	 * @return The requested document's text
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "getExtractedText")
	@WebResult(name = "text")
	@WSDoc(description = "gets the document's text stored in the full-text index")
	public String getExtractedText(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "the document id")
	@WebParam(name = "docId")
	long docId) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Gets the content of a specific version of a document
	 * 
	 * @param sid identifier of the session
	 * @param docId The document id
	 * @param version The specific version(it can be empty)
	 * 
	 * @return The requested version's binary
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 * @throws IOException I/O error
	 */
	@WebMethod(action = "getVersionContent")
	@WSDoc(description = "gets the content of a specific version of a document; returns the raw content of the file")
	public DataHandler getVersionContent(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "the document id")
	@WebParam(name = "docId")
	long docId, @WSDoc(description = "the version to retrieve, eg: '1.0', '2.3'")
	@WebParam(name = "version")
	String version)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException, IOException;

	/**
	 * Gets the content of a resource associated to the given document.
	 * 
	 * @param sid identifier of the session
	 * @param docId The document id
	 * @param fileVersion The specific file version(it can be empty)
	 * @param suffix Suffix specification(it can be empty, conversion.pdf to get
	 *        the PDF conversion)
	 * 
	 * @return The requested resource's binary
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 * @throws IOException I/O error
	 */
	@WebMethod(action = "getResource")
	@WSDoc(description = "gets the content of a resource associated to the given document; returns the raw content of the file")
	public DataHandler getResource(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "the document id")
	@WebParam(name = "docId")
	long docId, @WSDoc(description = "the file version to retrieve, eg: '1.0', '2.3'")
	@WebParam(name = "fileVersion")
	String fileVersion,
			@WSDoc(description = "suffix specification(it cannot be empty, use 'conversion.pdf' to get the PDF conversion)")
			@WebParam(name = "suffix")
			String suffix)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException, IOException;

	/**
	 * Checks out an existing document with the given identifier.
	 * 
	 * @param sid identifier of the session
	 * @param docId The document id
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod(action = "checkout")
	@WSDoc(description = "checks out an existing document with the given identifier")
	public void checkout(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "the document id")
	@WebParam(name = "docId")
	long docId) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Checks in an existing document with the given identifier to create a new
	 * version.
	 * 
	 * @param sid identifier of the session
	 * @param docId The document id
	 * @param comment The check in operation comment
	 * @param filename The document file name
	 * @param release True if this is a new release(eg: 2.0) rather than a
	 *        subversion(eg: 1.1)
	 * @param content The document's binary content
	 * 
	 * @throws IOException I/O error
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod(action = "checkin")
	@WSDoc(description = "checks in an existing document to create a new version")
	public void checkin(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "the document id")
	@WebParam(name = "docId")
	long docId, @WebParam(name = "comment")
	String comment, @WebParam(name = "filename")
	String filename, @WSDoc(description = "true if this is a new release(eg: 2.0) rather than a subversion(eg: 1.1)")
	@WebParam(name = "release")
	boolean release, @WebParam(name = "content")
	DataHandler content)
			throws IOException, AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	/**
	 * Checks in an existing document with the given identifier to create a new
	 * version.
	 * 
	 * @param sid identifier of the session
	 * @param docId The document id
	 * @param comment The check in operation comment
	 * @param filename The document file name
	 * @param release True if this is a new release(e.g.: 2.0) rather than a
	 *        subversion(e.g.: 1.1)
	 * @param docVO metadata for the document
	 * @param content The document's binary content
	 * 
	 * @throws IOException I/O error
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod(action = "checkinDocument")
	@WSDoc(description = "checks in an existing document to create a new version and also updates the metadata")
	public void checkinDocument(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "the document id")
	@WebParam(name = "docId")
	long docId, @WebParam(name = "comment")
	String comment, @WebParam(name = "filename")
	String filename, @WSDoc(description = "true if this is a new release(eg: 2.0) rather than a subversion(eg: 1.1)")
	@WebParam(name = "release")
	boolean release, @WSDoc(description = "optional, contains the metadata to update during the checkin")
	@WebParam(name = "docVO")
	WSDocument docVO, @WebParam(name = "content")
	DataHandler content)
			throws IOException, AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	/**
	 * Replaces the file associated to a given version.
	 * 
	 * @param sid identifier of the session
	 * @param docId The document id
	 * @param fileVersion The file version
	 * @param comment The comment left for this action
	 * @param content The file's binary content
	 *
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 * @throws IOException I/O error
	 */
	@WebMethod(action = "replaceFile")
	@WSDoc(description = "replaces the file associated to a given version")
	public void replaceFile(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "the document id")
	@WebParam(name = "docId")
	long docId, @WebParam(name = "fileVersion")
	String fileVersion, @WebParam(name = "comment")
	String comment, @WebParam(name = "content")
	DataHandler content)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException, IOException;

	/**
	 * Promotes an old version to the current default one. If you promote a
	 * prior version, what it does is make it the default version again.
	 * (regardless of there being many versions).
	 * 
	 * @param sid identifier of the session
	 * @param docId the document to be updated
	 * @param version the version
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 * @throws IOException I/O error
	 */
	@WebMethod(action = "promoteVersion")
	@WSDoc(description = "promotes an old version to the current default one")
	public void promoteVersion(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "the document id")
	@WebParam(name = "docId")
	long docId, @WebParam(name = "version")
	String version)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException, IOException;

	/**
	 * Creates a new document or updates an existing one.
	 * 
	 * @param sid identifier of the session
	 * @param docId The document id (optional)
	 * @param folderId The folder id (optional)
	 * @param release True if this is a major release(eg: 2.0) rather than a
	 *        minor release(eg: 1.12)
	 * @param filename The document file name
	 * @param language The language for the document
	 * @param content The document's binary content
	 * 
	 * @return The created/updated document's ID
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 * @throws IOException I/O erro
	 */
	@WebMethod(action = "upload")
	@WebResult(name = "docId")
	@WSDoc(description = "creates a new document or updates an existing one; returns the newly created document's ID")
	public long upload(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "id of the document to update")
	@WebParam(name = "docId")
	Long docId, @WSDoc(description = "the folder's id, used in case of creation", required = false)
	@WebParam(name = "folderId")
	Long folderId,
			@WSDoc(description = "true if this is a major release(eg: 2.0) rather than a minor release(eg: 1.12)")
			@WebParam(name = "release")
			boolean release, @WSDoc(description = "used in case of creation", required = false)
			@WebParam(name = "filename")
			String filename, @WebParam(name = "language")
			String language, @WSDoc(description = "raw content of the file")
			@WebParam(name = "content")
			DataHandler content)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException, IOException;

	/**
	 * Uploads a new resource attached to the given document. If the resource
	 * already exists it is overwritten.
	 * 
	 * @param sid Session identifier
	 * @param docId The document id
	 * @param fileVersion The specific file version(it can be empty)
	 * @param suffix Suffix specification(it cannot be empty, use
	 *        'conversion.pdf' to put the PDF conversion)
	 * @param content The resource's binary content
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 * @throws IOException I/O error
	 */
	@WebMethod(action = "uploadResource")
	@WSDoc(description = "uploads a new resource attached to the given document. If the resource already exists it is overwritten")
	public void uploadResource(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "the document id")
	@WebParam(name = "docId")
	long docId, @WSDoc(description = "the specific file version", required = false)
	@WebParam(name = "fileVersion")
	String fileVersion,
			@WSDoc(description = "suffix specification(it cannot be empty, use 'conversion.pdf' to put the PDF conversion)")
			@WebParam(name = "suffix")
			String suffix, @WSDoc(description = "raw content of the file")
			@WebParam(name = "content")
			DataHandler content)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException, IOException;

	/**
	 * Tests if a document is readable
	 * 
	 * @param sid identifier of the session
	 * @param docId The document id
	 * 
	 * @return True if the identifier denotes a document, otherwise false
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "isReadable")
	@WSDoc(description = "tests if a document is readable")
	public boolean isReadable(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "the document id")
	@WebParam(name = "docId")
	long docId) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Restores a deleted document
	 * 
	 * @param sid identifier of the session
	 * @param docId The document id
	 * @param folderId Id of the folder in which the document must be restored
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "restore")
	@WSDoc(description = "restores a deleted document")
	public void restore(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "the document id")
	@WebParam(name = "docId")
	long docId, @WSDoc(description = "id of the folder in which the document must be restored")
	@WebParam(name = "folderId")
	long folderId) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Gets the version history of an existing document with the given
	 * identifier
	 * 
	 * @param sid identifier of the session
	 * @param docId The document id
	 * 
	 * @return Array of versions
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod(action = "getVersions")
	@WebResult(name = "version")
	@WSDoc(description = "gets the versions' history of a document; returns an array of versions")
	public WSDocument[] getVersions(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "the document id")
	@WebParam(name = "docId")
	long docId) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Gets a specific version
	 * 
	 * @param sid identifier of the session
	 * @param docId The document id
	 * @param version The version number
	 * 
	 * @return The version
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod(action = "getVersion")
	@WebResult(name = "version")
	@WSDoc(description = "gets a version of a document; returns the version")
	public WSDocument getVersion(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "the document id")
	@WebParam(name = "docId")
	long docId, @WSDoc(description = "the version number")
	@WebParam(name = "version")
	String version) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Gets a document in a specific folder
	 * 
	 * @param sid identifier of the session
	 * @param folderId The folder id
	 * @param fileName Optional file name filter
	 * 
	 * @return A value object containing the document's metadata
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod(action = "listDocuments")
	@WebResult(name = "document")
	@WSDoc(description = "gets the documents in a specific folder")
	public WSDocument[] listDocuments(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "folderId")
	long folderId, @WSDoc(description = "file name filter", required = false)
	@WebParam(name = "fileName")
	String fileName) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Lists of last modified documents of the current session.
	 * 
	 * @param sid identifier of the session
	 * @param maxHits Maximum number of returned records
	 * 
	 * @return Array of documents
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "getRecentDocuments")
	@WebResult(name = "document")
	@WSDoc(description = "lists of last modified documents in the current session")
	public WSDocument[] getRecentDocuments(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "max number of returned records", required = false)
	@WebParam(name = "maxHits")
	Integer maxHits) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Sends a set of documents as mail attachments
	 * 
	 * @param sid identifier of the session
	 * @param docIds Set of document identifiers
	 * @param recipients Set of recipients(comma separated)
	 * @param subject The email subject
	 * @param message The email message body
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws IOException I/O error
	 * @throws MessagingException error sending the email
	 */
	@WebMethod(action = "sendEmail")
	@WSDoc(description = "sends a set of documents as mail attachments")
	public void sendEmail(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "docIds")
	Long[] docIds, @WSDoc(description = "comma separated list of email addresses")
	@WebParam(name = "recipients")
	String recipients, @WebParam(name = "subject")
	String subject, @WebParam(name = "message")
	String message)
			throws AuthenticationException, WebserviceException, PersistenceException, IOException, MessagingException;

	/**
	 * Creates a new document alias for the given document inside a specified
	 * folder
	 * 
	 * @param sid identifier of the session
	 * @param docId The original document id
	 * @param folderId Identifier of the folder in which will be stored the
	 *        alias.
	 * @param type Type of the alias
	 * 
	 * @return The value object containing the document's metadata
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod(action = "createAlias")
	@WebResult(name = "document")
	@WSDoc(description = "creates a new document alias for the given document inside a specified folder")
	public WSDocument createAlias(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "the original document's id")
	@WebParam(name = "docId")
	long docId, @WebParam(name = "folderId")
	long folderId,
			@WSDoc(description = "type of the alias (use 'pdf' to create an alias to the PDF conversion)", required = false)
			@WebParam(name = "type")
			String type) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Creates a new link between two documents.
	 * 
	 * @param sid identifier of the session
	 * @param doc1 ID of document 1
	 * @param doc2 ID of document 2
	 * @param type The link type(it can be empty)
	 * 
	 * @return the new link
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod(action = "link")
	@WebResult(name = "link")
	@WSDoc(description = "creates a new link between two documents")
	public WSLink link(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "ID of document 1")
	@WebParam(name = "doc1")
	long doc1, @WSDoc(description = "ID of document 2")
	@WebParam(name = "doc2")
	long doc2, @WSDoc(description = "type of the link (use 'pdf' to point to the pdf conversion)", required = false)
	@WebParam(name = "type")
	String type) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Gets all the links of a specific document
	 * 
	 * @param sid Session identifier
	 * @param docId ID of the document
	 * 
	 * @return The new links of the document
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod(action = "getLinks")
	@WebResult(name = "link")
	@WSDoc(description = "gets all the links of a specific document; returns an array of links")
	public WSLink[] getLinks(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "the document id")
	@WebParam(name = "docId")
	long docId) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Removes an existing link
	 * 
	 * @param sid identifier of the session
	 * @param id ID of the link
	 * 
	 * @throws PersistenceException Error at data layer
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "deleteLink")
	@WSDoc(description = "removes an existing link")
	public void deleteLink(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "identifier of the link")
	@WebParam(name = "id")
	long id) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Re-indexes(or indexes from scratch) a document
	 * 
	 * @param sid Session identifier
	 * @param docId The document id
	 * @param content The content to be used (if null the file is parsed)
	 * 
	 * @throws PersistenceException Error at data layer
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws ParseException Error in adding the entry into fulltext index
	 */
	@WebMethod(action = "reindex")
	@WSDoc(description = "re-indexes(or indexes from scratch) a document")
	public void reindex(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "the document id")
	@WebParam(name = "docId")
	long docId, @WSDoc(description = "the content to be used (if null the file is parsed)", required = false)
	@WebParam(name = "content")
	String content) throws AuthenticationException, WebserviceException, PersistenceException, ParseException;

	/**
	 * Creates the PDF conversion of the given document. If the conversion was
	 * already created, nothing will happen.
	 * 
	 * @param sid identifier of the session
	 * @param docId The document id
	 * @param fileVersion The specific file version(it can be empty)
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 * @throws IOException I/O error
	 */
	@WebMethod(action = "createPdf")
	@WSDoc(description = "creates the PDF conversion of the given document; if the conversion was already created, nothing will happen")
	public void createPdf(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "the document id")
	@WebParam(name = "docId")
	long docId, @WSDoc(description = "the specific file version", required = false)
	@WebParam(name = "fileVersion")
	String fileVersion)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException, IOException;

	/**
	 * Creates the thumbnail of the given document. If the thumbnail was already
	 * created, nothing will happen.
	 * 
	 * @param sid Session identifier
	 * @param docId The document id
	 * @param fileVersion The specific file version(it can be empty)
	 * @param type The thumbnail type(eg: thumb, tile, mobile, thumb<b>XXX</b>
	 *        where <b>XXX</b> is a resolution in pixels)
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws IOException I/O error
	 */
	@WebMethod(action = "createThumbnail")
	@WSDoc(description = "creates the thumbail of the given document; if the thumbnail was already created, nothing will happen")
	public void createThumbnail(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "the document id")
	@WebParam(name = "docId")
	long docId, @WSDoc(description = "the specific file version", required = false)
	@WebParam(name = "fileVersion")
	String fileVersion,
			@WSDoc(description = "the thumbnail type(eg: thumbnail, tile, mobile, thumbXXX)", required = false)
			@WebParam(name = "type")
			String type) throws AuthenticationException, WebserviceException, PersistenceException, IOException;

	/**
	 * Creates a new download ticket
	 * 
	 * @param sid identifier of the session
	 * @param docId identifier of the document
	 * @param suffix can be null or 'conversion.pdf'
	 * @param expireHours expiration time expressed in hours
	 * @param expireDate exact expiration date expressed in the format
	 *        yyyy-MM-dd
	 * @param maxDownloads maximum number of downloads allowed
	 * 
	 * @return the download ticket
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the download
	 *         permission
	 */
	@WebResult(name = "ticket")
	@WebMethod(action = "createDownloadTicket")
	@WSDoc(description = "creates a new download ticket to the original document or it's PDF conversion")
	public String createDownloadTicket(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "the document id", required = true)
	@WebParam(name = "docId")
	long docId, @WSDoc(description = "can be null or 'conversion.pdf'", required = false)
	@WebParam(name = "suffix")
	String suffix, @WSDoc(description = "expiration time expressed in hours", required = false)
	@WebParam(name = "expireHours")
	Integer expireHours,
			@WSDoc(description = "exact expiration date expressed in the format yyyy-MM-dd", required = false)
			@WebParam(name = "expireDate")
			String expireDate, @WSDoc(description = "maximum number of downloads allowed", required = false)
			@WebParam(name = "maxDownloads")
			Integer maxDownloads)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Creates a new view ticket
	 * 
	 * @param sid identifier of the session
	 * @param docId identifier of the document
	 * @param suffix can be null or 'conversion.pdf'
	 * @param expireHours expiration time expressed in hours
	 * @param expireDate exact expiration date expressed in the format
	 *        yyyy-MM-dd
	 * @param maxDownloads maximum number of downloads allowed
	 * @param maxViews maximum number of downloads allowed
	 * 
	 * @return the download ticket
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the download
	 *         permission
	 */
	@WebResult(name = "ticket")
	@WebMethod(action = "createViewTicket")
	@WSDoc(description = "creates a new view ticket to the original document or it's PDF conversion")
	public String createViewTicket(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "the document id", required = true)
	@WebParam(name = "docId")
	long docId, @WSDoc(description = "can be null or 'conversion.pdf'", required = false)
	@WebParam(name = "suffix")
	String suffix, @WSDoc(description = "expiration time expressed in hours", required = false)
	@WebParam(name = "expireHours")
	Integer expireHours,
			@WSDoc(description = "exact expiration date expressed in the format yyyy-MM-dd", required = false)
			@WebParam(name = "expireDate")
			String expireDate, @WSDoc(description = "maximum number of downloads allowed", required = false)
			@WebParam(name = "maxDownloads")
			Integer maxDownloads, @WSDoc(description = "maximum number of views allowed", required = false)
			@WebParam(name = "maxViews")
			Integer maxViews)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Puts a password protection to the document
	 * 
	 * @param sid identifier of the session
	 * @param docId identifier of the document
	 * @param password the new password
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod(action = "setPassword")
	@WSDoc(description = "protects with a password the given document")
	public void setPassword(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "the document id")
	@WebParam(name = "docId")
	long docId, @WebParam(name = "password")
	String password) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Removes the password protection from the document
	 * 
	 * @param sid identifier of the session
	 * @param docId identifier of the document
	 * @param currentPassword the password
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod(action = "unsetPassword")
	@WSDoc(description = "removes the password protection from the document")
	public void unsetPassword(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "the document id")
	@WebParam(name = "docId")
	long docId, @WebParam(name = "currentPassword")
	String currentPassword)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Unprotects a document that is password protected. If the given password
	 * is right, the document remains unprotected for the duration of the
	 * session
	 * 
	 * @param sid identifier of the session
	 * @param docId identifier of the document
	 * @param password the password
	 * 
	 * @return was it uprotected?
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebResult(name = "unprotect")
	@WebMethod(action = "unprotect")
	@WSDoc(description = "unprotects a document that is password protected. If the given password is right, the document remains unprotected for the duration of the session.")
	public boolean unprotect(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "the document id")
	@WebParam(name = "docId")
	long docId, @WebParam(name = "password")
	String password) throws PersistenceException, AuthenticationException, WebserviceException;

	/**
	 * Adds a new note for the given document
	 * 
	 * @param sid identifier of the session
	 * @param docId identifier of the document
	 * @param note text of the note
	 *
	 * @return the created note
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod(action = "addNote")
	@WebResult(name = "note")
	@WSDoc(description = "adds a new note for the given document")
	public WSNote addNote(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "the document id", required = true)
	@WebParam(name = "docId")
	long docId, @WebParam(name = "note")
	String note) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Adds a new note for the given document
	 * 
	 * @param sid identifier of the session
	 * @param docId identifier of the document
	 * @param note the note to add
	 * 
	 * @return the added note
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod(action = "saveNote")
	@WebResult(name = "note")
	@WSDoc(description = "adds/modifies a note for the given document")
	public WSNote saveNote(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "the document id")
	@WebParam(name = "docId")
	long docId, @WebParam(name = "note")
	WSNote note) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Adds a new note for the given document
	 * 
	 * @param sid identifier of the session
	 * @param noteId identifier of the note
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "deleteNote")
	@WSDoc(description = "deletes a note, only the author or the administrator can delete the note")
	public void deleteNote(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "noteId")
	long noteId) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Gets the notes for the given document
	 * 
	 * @param sid identifier of the session
	 * @param docId identifier of the document
	 * 
	 * @return array of notes
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod(action = "getNotes")
	@WebResult(name = "note")
	@WSDoc(description = "gets the notes for the given document")
	public WSNote[] getNotes(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "the document id")
	@WebParam(name = "docId")
	long docId) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Deletes a version of a document with the given identifiers. You can not
	 * delete the latest version of a document
	 * 
	 * @param sid identifier of the session
	 * @param docId The document id
	 * @param version The specific version
	 * 
	 * @return the latest version specification
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "deleteVersion")
	@WebResult(name = "latest-version")
	@WSDoc(description = "deletes a version of a document with the given identifiers")
	public String deleteVersion(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "the document id")
	@WebParam(name = "docId")
	long docId, @WSDoc(description = "the version to retrieve, eg: '1.0', '2.3'")
	@WebParam(name = "version")
	String version) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Puts a new rating on the given document
	 * 
	 * @param sid identifier of the session
	 * @param docId identifier of the document
	 * @param vote the vote
	 * 
	 * @return the rating
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod(action = "rateDocument")
	@WebResult(name = "rating")
	@WSDoc(description = "rates the given document")
	public WSRating rateDocument(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "the document id")
	@WebParam(name = "docId")
	long docId, @WebParam(name = "vote")
	int vote) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Gets all the ratings of the given document
	 * 
	 * @param sid identifier of the session
	 * @param docId identifier of the document
	 * 
	 * @return array of ratings
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod(action = "getRatings")
	@WebResult(name = "rating")
	@WSDoc(description = "gets all the ratings of the given document")
	public WSRating[] getRatings(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "the document id")
	@WebParam(name = "docId")
	long docId) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Grants user permission to the document.
	 * 
	 * @param sid Session identifier
	 * @param docId Document id
	 * @param userId User Id
	 * @param permissions the permission integer representation. If '0', the
	 *        user will be not granted to access the document.
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod(action = "grantUser")
	@WSDoc(description = "grants user permission to the document")
	public void grantUser(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "docId")
	long docId, @WebParam(name = "userId")
	long userId,
			@WSDoc(description = "the permission integer representation; if '0', the user will be not granted to access the document")
			@WebParam(name = "permissions")
			int permissions)
			throws PersistenceException, PermissionException, AuthenticationException, WebserviceException;

	/**
	 * Grants group permission to the document
	 * 
	 * @param sid Session identifier
	 * @param docId Document id
	 * @param groupId Group Id
	 * @param permissions the permission integer representation. If '0', the
	 *        group will be not granted to access the document.
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod(action = "grantGroup")
	@WSDoc(description = "grants group permission to the document")
	public void grantGroup(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "folderId")
	long docId, @WebParam(name = "groupId")
	long groupId,
			@WSDoc(description = "the permission integer representation; if '0', the group will be not granted to access the document")
			@WebParam(name = "permissions")
			int permissions)
			throws PermissionException, PersistenceException, AuthenticationException, WebserviceException;

	/**
	 * Retrieves the list of granted users for the given document.
	 * 
	 * @param sid Session identifier
	 * @param docId Document id
	 * 
	 * @return 'error' if error occurred, the right objects collection.
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "getGrantedUsers")
	@WSDoc(description = "retrieves the list of granted users for the given document")
	public WSRight[] getGrantedUsers(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "docId")
	long docId) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Retrieves the list of granted groups for the given document
	 * 
	 * @param sid Session identifier
	 * @param docId Document id
	 * @return 'error' if error occurred, the right objects collection
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "getGrantedGroups")
	@WSDoc(description = "retrieves the list of granted groups for the given folder")
	public WSRight[] getGrantedGroups(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "docId")
	long docId) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Tests if a document is readable.
	 * 
	 * @param sid Session identifier
	 * @param docId The document id
	 * 
	 * @return True if the identifier denotes a readable document, otherwise
	 *         false.
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "isRead")
	@WSDoc(description = "tests if a document is readable")
	public boolean isRead(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "docId")
	long docId) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Tests if a document is writable
	 * 
	 * @param sid Session identifier
	 * @param docId The document id
	 * @return True if the identifier denotes a writable folder, otherwise false
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "isWrite")
	@WSDoc(description = "tests if a folder is writable")
	public boolean isWrite(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "docId")
	long docId) throws AuthenticationException, WebserviceException, PersistenceException;
	
	/**
	 * Tests if a document is downloadable
	 * 
	 * @param sid Session identifier
	 * @param docId The document id
	 * @return True if the identifier denotes a writable folder, otherwise false
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "isDownload")
	@WSDoc(description = "tests if a folder is writable")
	public boolean isDownload(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "docId")
	long docId) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Tests if the current user has a specific permission on a document
	 * 
	 * @param sid Session identifier
	 * @param docId The document id
	 * @param permission The permission representation
	 * 
	 * @return True if the identifier denotes a granted permission, otherwise
	 *         false
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "isGranted")
	@WSDoc(description = "tests if the current user has a specific permission on a document")
	public boolean isGranted(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "docId")
	long docId, @WSDoc(description = "the permissions' integer representation")
	@WebParam(name = "permission")
	int permission) throws AuthenticationException, WebserviceException, PersistenceException;

}