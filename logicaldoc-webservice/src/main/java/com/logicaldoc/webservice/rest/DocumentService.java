package com.logicaldoc.webservice.rest;

import java.io.IOException;
import java.util.List;

import javax.activation.DataHandler;
import javax.mail.MessagingException;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.PathSegment;
import javax.ws.rs.core.Response;

import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.Multipart;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.parser.ParsingException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.authorization.UnexistingResourceException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSAccessControlEntry;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSLink;
import com.logicaldoc.webservice.model.WSNote;
import com.logicaldoc.webservice.model.WSRating;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;

@Consumes({ MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON })
@Produces({ MediaType.APPLICATION_JSON })
public interface DocumentService {

	/**
	 * Creates a new document
	 * 
	 * @param document the document's metadata
	 * @param contentDetail the file binaries
	 * 
	 * @return data structure representing the created document
	 */
	@POST
	@Path("/create")
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@Produces(MediaType.APPLICATION_JSON)
	@Operation(summary = "Creates a new document")
	@ApiResponses(value = {
			@ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = WSDocument.class))),
			@ApiResponse(responseCode = "401", description = "Authentication failed"),
			@ApiResponse(responseCode = "500", description = "Generic error, see the response message") })
	public WSDocument create(@Multipart(value = "document", required = true, type = "application/json")
	WSDocument document, @Multipart(value = "content", required = true, type = "application/octet-stream")
	Attachment contentDetail);

	/**
	 * Retrieves a document from the database
	 * 
	 * @param docId identifier of the document
	 * @return the document object representation
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 */
	@GET
	@Path("/getDocument")
	@Produces({ MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
	WSDocument getDocument(@QueryParam("docId")
	long docId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	/**
	 * Executed the checkout
	 * 
	 * @param docId identifier of the document
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 */
	@POST
	@Path("/checkout")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	void checkout(@FormParam("docId")
	long docId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	/**
	 * Check-in an existing document
	 *
	 * Performs a check-in (commit) operation of new content over an existing
	 * document. The document must be in checked-out status
	 * 
	 * @param docId identifier of the document
	 * @param comment version comment
	 * @param release it this is a major version or not
	 * @param filename filename of the document
	 * @param filedataDetail binary content of the file
	 */
	@POST
	@Path("/checkin")
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@Operation(summary = "Check-in an existing document", description = "Performs a check-in (commit) operation of new content over an existing document. The document must be in checked-out status")
	@ApiResponses(value = { @ApiResponse(responseCode = "204", description = "Successful operation"),
			@ApiResponse(responseCode = "401", description = "Authentication failed"),
			@ApiResponse(responseCode = "500", description = "Generic error, see the response message") })
	public void checkin(@Multipart(value = "docId", required = true)
	String docId, @Multipart(value = "comment", required = false)
	String comment, @Multipart(value = "release", required = false)
	String release, @Multipart(value = "filename", required = true)
	String filename, @Multipart(value = "filedata", required = true)
	Attachment filedataDetail);

	/**
	 * Replace the file of a version. Replaces the file associated to a given
	 * version.
	 * 
	 * @param docId identifier of the document
	 * @param comment version comment
	 * @param fileVersion the file version
	 * @param filedataDetail binary content of the file
	 * 
	 * 
	 */
	@POST
	@Path("/replaceFile")
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@Operation(summary = "Replace the file of a version", description = "Replaces the file associated to a given version.")
	@ApiResponses(value = { @ApiResponse(responseCode = "204", description = "successful operation"),
			@ApiResponse(responseCode = "400", description = "bad request"),
			@ApiResponse(responseCode = "401", description = "Authentication failed"),
			@ApiResponse(responseCode = "500", description = "Generic error, see the response message") })
	public void replaceFile(
			@Multipart(value = "docId", required = false) String docId, 
			@Multipart(value = "fileVersion", required = false)	String fileVersion, 
			@Multipart(value = "comment", required = false)	String comment, 
			@Multipart(value = "filedata", required = false) Attachment filedataDetail);

	/**
	 * Uploads a document
	 *
	 * Creates or updates an existing document, if used in update mode docId
	 * must be provided, when used in create mode folderId is required. Returns
	 * the ID of the created/updated document. &lt;br/&gt;Example: curl -u
	 * admin:admin -H &#x27;&#x27;Accept: application/json&#x27;&#x27; -X POST
	 * -F folderId&#x3D;4 -F filename&#x3D;newDoc.txt -F
	 * filedata&#x3D;@newDoc.txt
	 * http://localhost:8080/services/rest/document/upload
	 * 
	 * @param docId identifier of the document (string format)
	 * @param folderId identifier of the folder
	 * @param release if the upload must produce a major release or now
	 * @param filename name of the file
	 * @param language the document's language
	 * @param filedataDetail the binary content
	 * 
	 * @return identifier of the updated document
	 * 
	 * 
	 */
	@POST
	@Path("/upload")
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@Operation(summary = "Uploads a document", description = "Creates or updates an existing document, if used in update mode docId must be provided, when used in create mode folderId is required. Returns the ID of the created/updated document. &lt;br/&gt;Example: curl -u admin:admin -H ''Accept: application/json'' -X POST -F folderId=4 -F filename=newDoc.txt -F filedata=@newDoc.txt http://localhost:8080/services/rest/document/upload")
	@ApiResponses(value = {
			@ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Long.class, description = "The ID of the document created or updated"))),
			@ApiResponse(responseCode = "401", description = "Authentication failed"),
			@ApiResponse(responseCode = "500", description = "Generic error, see the response message") })
	public Long upload(@Multipart(value = "docId", required = false)
	String docId, @Multipart(value = "folderId", required = false)
	String folderId, @Multipart(value = "release", required = false)
	String release, @Multipart(value = "filename", required = true)
	String filename, @Multipart(value = "language", required = false)
	String language, @Multipart(value = "filedata", required = true)
	Attachment filedataDetail);

	/**
	 * Deletes a document
	 * 
	 * @param docId identifier of the document to delete
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 */
	@DELETE
	@Path("/delete")
	void delete(@QueryParam("docId")
	long docId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	/**
	 * Lists the documents in a folder
	 * 
	 * @param folderId identifier of the folder
	 * 
	 * @return array of documents contained in the folder
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 */
	@GET
	@Path("/list")
	@Produces({ MediaType.APPLICATION_JSON })
	List<WSDocument> list(@QueryParam("folderId")
	long folderId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;
	
	/**
	 * Gets the documents in a specific folder
	 * 
	 * @param folderId The folder id
	 * @param fileName Optional file name filter
	 * @param sort Optional sort criteria
	 * @param page Optional page number
	 * @param max Optional maximum number of elements per page
	 * 
	 * @return Collection of documents
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@GET
	@Path("/listPaginated")
	@Produces({ MediaType.APPLICATION_JSON })
	List<WSDocument> listPaginated(@QueryParam("folderId")
	long folderId, @QueryParam("fileName")
	String fileName, @QueryParam("sort")
	String sort, @QueryParam("page")
	Integer page, @QueryParam("max")
	Integer max) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	/**
	 * Lists the documents in a folder
	 * 
	 * @param folderId identifier of the folder
	 * @param fileName a file name to use as filter
	 * 
	 * @return array of documents contained in the folder
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 */
	@GET
	@Path("/listDocuments")
	List<WSDocument> listDocuments(@QueryParam("folderId")
	long folderId, @QueryParam("fileName")
	String fileName) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	/**
	 * Updates an existing document with the value object containing the
	 * document's metadata.
	 * 
	 * @param document the document to update
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 * @throws UnexistingResourceException The specified document does not exist
	 */
	@PUT
	@Path("/update")
	void update(WSDocument document) throws AuthenticationException, PermissionException, WebserviceException,
			PersistenceException, UnexistingResourceException;

	/**
	 * Retrieves the file content of a document.
	 * 
	 * @param docId identifier of the document
	 * 
	 * @return the contents
	 * 
	 * @throws IOException I/O error
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 */
	@GET
	@Path("/getContent")
	@Produces(MediaType.APPLICATION_OCTET_STREAM)
	DataHandler getContent(@QueryParam("docId")
	long docId)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException, IOException;

	/**
	 * Retrieves the file content of a version.
	 * 
	 * @param docId identifier of the document
	 * @param version version specification
	 * 
	 * @return the contents
	 * 
	 * @throws IOException I/O error
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 */
	@GET
	@Path("/getVersionContent")
	@Produces(MediaType.APPLICATION_OCTET_STREAM)
	DataHandler getVersionContent(@QueryParam("docId")
	long docId, @QueryParam("version")
	String version)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException, IOException;

	/**
	 * Adds a new note for the given document
	 * 
	 * @param docId identifier of the document
	 * @param note the note to add
	 * 
	 * @return the added note
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 * @throws UnexistingResourceException The specified document does not exist
	 */
	@POST
	@Path("/addNote")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	public WSNote addNote(@FormParam("docId")
	long docId, @FormParam("note")
	String note) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			UnexistingResourceException;

	/**
	 * Deletes a new note by note identifier
	 * 
	 * @param noteId identifier of the note
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws AuthenticationException Invalid credentials
	 */
	@DELETE
	@Path("/deleteNote")
	public void deleteNote(@QueryParam("noteId")
	long noteId) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Gets the notes for the given document
	 * 
	 * @param docId identifier of the document
	 *
	 * @return List of notes
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 * @throws UnexistingResourceException The specified document does not exist
	 */
	@GET
	@Path("/getNotes")
	public List<WSNote> getNotes(@QueryParam("docId")
	long docId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			UnexistingResourceException;

	/**
	 * Puts a new rating on the given document
	 * 
	 * @param docId identifier of the document
	 * @param vote the vote
	 * 
	 * @return the voted document
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 * @throws UnexistingResourceException The specified document does not exist
	 */
	@PUT
	@Path("/rateDocument")
	public WSRating rateDocument(@QueryParam("docId")
	long docId, @QueryParam("vote")
	int vote) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			UnexistingResourceException;

	/**
	 * Gets all the ratings of the given document
	 * 
	 * @param docId identifier of the document
	 * 
	 * @return the ratings
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 * @throws UnexistingResourceException The specified document does not exist
	 */
	@GET
	@Path("/getRatings")
	public List<WSRating> getRatings(@QueryParam("docId")
	long docId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			UnexistingResourceException;

	/**
	 * Deletes a version by document identifier and version ID. You can not
	 * delete the latest version of a document
	 * 
	 * @param docId identifier of the document
	 * @param version the document's version
	 * 
	 * @return the latest version of the document
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws AuthenticationException Invalid credentials
	 */
	@DELETE
	@Path("/deleteVersion")
	public String deleteVersion(@QueryParam("docId")
	long docId, @QueryParam("version")
	String version) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Moves an existing document with the given identifier
	 * 
	 * @param docId The document id
	 * @param folderId Identifier of the new document's folder
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 * @throws UnexistingResourceException The requested document does not exist
	 */
	@PUT
	@Path("/move")
	public void move(@QueryParam("docId")
	long docId, @QueryParam("folderId")
	long folderId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			UnexistingResourceException;

	/**
	 * Copies a document into another folder
	 * 
	 * @param docId The document id
	 * @param folderId Identifier of the new document's folder
	 * @param links If links must be copied too
	 * @param notes If notes and annotations must be copied too
	 *
	 * @return The new copy
	 * 
	 * @throws IOException I/O error
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 */
	@PUT
	@Path("/copy")
	@Produces(MediaType.APPLICATION_JSON)
	@Operation(summary = "Copies a document")
	@ApiResponses(value = {
			@ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = WSDocument.class))),
			@ApiResponse(responseCode = "401", description = "Authentication failed"),
			@ApiResponse(responseCode = "500", description = "Generic error, see the response message") })
	public WSDocument copy(@QueryParam("docId")
	long docId, @QueryParam("folderId")
	long folderId, @QueryParam("links")
	boolean links, @QueryParam("notes")
	boolean notes, @QueryParam("security")
	boolean security)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException, IOException;

	/**
	 * Creates the thumbail of the given document; if the thumbnail was already
	 * created, nothing will happen
	 * 
	 * @param docId The document id
	 * @param fileVersion The specific file version(it can be empty)
	 * @param type The thumbnail type(eg: thumbnail, tile, mobile)
	 * 
	 * @throws IOException I/O error
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws AuthenticationException Invalid credentials
	 */
	@PUT
	@Path("/createThumbnail")
	public void createThumbnail(@QueryParam("docId")
	long docId, @QueryParam("fileVersion")
	String fileVersion, @QueryParam("type")
	String type) throws AuthenticationException, WebserviceException, PersistenceException, IOException;

	/**
	 * Retrieves the thumbnail image
	 * 
	 * @param type type of the thumbnail
	 * @param docPath path of the document
	 * @param docPathList path of the document
	 * 
	 * @return image content
	 * 
	 * @throws IOException I/O error
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 */
	@GET
	@Path("/thumbnail/{type}/{docpath:.*}")
	@Produces("image/jpeg")
	public Response getThumbnail(@PathParam("type")
	String type, @PathParam("docpath")
	String docPath, @PathParam("docpath")
	List<PathSegment> docPathList)
			throws AuthenticationException, WebserviceException, PersistenceException, IOException, PermissionException;

	/**
	 * Creates the PDF conversion of the given document; if the PDF conversion
	 * was already created, nothing will happen
	 * 
	 * @param docId The document id
	 * @param fileVersion The specific file version(it can be empty)
	 * 
	 * @throws IOException I/O error
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 */
	@PUT
	@Path("/createPdf")
	public void createPdf(@QueryParam("docId")
	long docId, @QueryParam("fileVersion")
	String fileVersion)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException, IOException;

	/**
	 * Promotes an old version to the current default one. If you promote a
	 * prior version, what it does is make it the default version again.
	 * (regardless of there being many versions).
	 * 
	 * @param docId the document to be updated
	 * @param version the version
	 * 
	 * @throws IOException I/O error
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 * @throws UnexistingResourceException The specified document does not exist
	 */
	@PUT
	@Path("/promoteVersion")
	public void promoteVersion(@QueryParam("docId")
	long docId, @QueryParam("version")
	String version) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			IOException, UnexistingResourceException;

	/**
	 * Renames the title of an existing document with the given identifier.
	 * 
	 * @param docId The document id
	 * @param name The new document filename
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 * @throws UnexistingResourceException The specified document does not exist
	 */
	@PUT
	@Path("/rename")
	public void rename(@QueryParam("docId")
	long docId, @QueryParam("name")
	String name) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			UnexistingResourceException;

	/**
	 * Gets the version history of an existing document with the given
	 * identifier
	 * 
	 * @param docId The document id
	 * 
	 * @return Array of versions
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 * @throws UnexistingResourceException The specified document does not exist
	 */
	@GET
	@Path("/getVersions")
	public List<WSDocument> getVersions(@QueryParam("docId")
	long docId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			UnexistingResourceException;

	/**
	 * Creates a new document alias for the given document inside a specified
	 * folder
	 * 
	 * @param docId The original document id
	 * @param folderId Identifier of the folder in which will be stored the
	 *        alias.
	 * @param type Type of the alias
	 * 
	 * @return The value object containing the document's metadata
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 */
	@POST
	@Path("/createAlias")
	public WSDocument createAlias(long docId, long folderId, String type)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	/**
	 * Creates a new download ticket
	 * 
	 * @param docId identifier of the document
	 * @param suffix can be null or 'conversion.pdf'
	 * @param expireHours expiration time expressed in hours
	 * @param expireDate exact expiration date expressed in the format
	 *        yyyy-MM-dd
	 * @param maxDownloads maximum number of admitted downloads
	 * 
	 * @return the download ticket
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws AuthenticationException Invalid credentials
	 * @throws PermissionException The user does not have the download
	 *         permission
	 */
	@POST
	@Path("/createDownloadTicket")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	public String createDownloadTicket(long docId, String suffix, Integer expireHours, String expireDate,
			Integer maxDownloads)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Creates a new download ticket
	 * 
	 * @param docId identifier of the document
	 * @param suffix can be null or 'conversion.pdf'
	 * @param expireHours expiration time expressed in hours
	 * @param expireDate exact expiration date expressed in the format
	 *        yyyy-MM-dd
	 * @param maxDownloads maximum number of admitted downloads
	 * @param maxViews maximum number of admitted views
	 * 
	 * @return the download ticket
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws AuthenticationException Invalid credentials
	 * @throws PermissionException The user does not have the download
	 *         permission
	 */
	@POST
	@Path("/createViewTicket")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	public String createViewTicket(long docId, String suffix, Integer expireHours, String expireDate,
			Integer maxDownloads, Integer maxViews)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Removes an existing link
	 * 
	 * @param id ID of the link
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws AuthenticationException Invalid credentials
	 */
	@DELETE
	@Path("/deleteLink")
	public void deleteLink(long id) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Gets the aliases of the given document
	 * 
	 * @param docId The master document ID
	 * @return List of aliases
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws AuthenticationException Invalid credentials
	 */
	@GET
	@Path("/getAliases")
	public List<WSDocument> getAliases(long docId)
			throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Gets document metadata of an existing document with the given custom
	 * identifier
	 * 
	 * @param customId The custom id
	 * 
	 * @return A value object containing the document's metadata
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 */
	@GET
	@Path("/getDocumentByCustomId")
	public WSDocument getDocumentByCustomId(String customId)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	/**
	 * Gets document metadata of a collection of existing documents with the
	 * given identifiers
	 * 
	 * @param docIds identifiers of the documents
	 * 
	 * @return the list of documents
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws AuthenticationException Invalid credentials
	 */
	@GET
	@Path("/getDocuments")
	public List<WSDocument> getDocuments(List<Long> docIds)
			throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Gets the document's text stored in the full-text index
	 * 
	 * @param docId The document id
	 * 
	 * @return The requested document's text
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws AuthenticationException Invalid credentials
	 */
	@GET
	@Path("/getExtractedText")
	@Produces({ MediaType.TEXT_PLAIN })
	public String getExtractedText(long docId)
			throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Lists of last modified documents of the current session
	 * 
	 * @param maxHits Maximum number of returned records
	 * 
	 * @return List of documents
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws AuthenticationException Invalid credentials
	 */
	@GET
	@Path("/getRecentDocuments")
	public List<WSDocument> getRecentDocuments(Integer maxHits)
			throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Gets all the links of a specific document
	 * 
	 * @param docId ID of the document
	 * 
	 * @return The new links of the document
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 */
	@GET
	@Path("/getLinks")
	public List<WSLink> getLinks(long docId)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	/**
	 * Gets the content of a resource associated to the given document.
	 * 
	 * @param docId The document id
	 * @param fileVersion The specific file version(it can be empty)
	 * @param suffix Suffix specification(it can be empty, conversion.pdf to get
	 *        the PDF conversion)
	 * 
	 * @return The requested resource's binary
	 * 
	 * @throws IOException I/O error
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 */
	@GET
	@Path("/getResource")
	@Produces(MediaType.APPLICATION_OCTET_STREAM)
	public DataHandler getResource(long docId, String fileVersion, String suffix)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException, IOException;

	/**
	 * Creates a new link between two documents.
	 * 
	 * @param doc1 ID of document 1
	 * @param doc2 ID of document 2
	 * @param type The link type(it can be empty)
	 * 
	 * @return the new link
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 * @throws UnexistingResourceException The specified document does not exist
	 */
	public WSLink link(long doc1, long doc2, String type) throws AuthenticationException, PermissionException,
			WebserviceException, PersistenceException, UnexistingResourceException;

	/**
	 * Locks an existing document with the given identifier.
	 * 
	 * @param docId The document id
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 * @throws UnexistingResourceException The specified document does not exist
	 */
	public void lock(long docId) throws AuthenticationException, PermissionException, WebserviceException,
			PersistenceException, UnexistingResourceException;

	/**
	 * Re-indexes(or indexes from scratch) a document
	 * 
	 * @param docId The document id
	 * @param content The content to be used (if null the file is parsed)
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws ParsingException Error parsing the content
	 * @throws AuthenticationException Invalid credentials
	 */
	public void reindex(long docId, String content)
			throws AuthenticationException, ParsingException, WebserviceException, PersistenceException;

	/**
	 * Uploads a new resource of the document
	 *
	 * Uploads a new resource attached to the given document. If the resource
	 * already exists it is overwritten
	 * 
	 * @param docId identifier of the document
	 * @param fileVersion version of the file
	 * @param suffix suffix specification
	 * @param contentDetail file content
	 * 
	 * @throws IOException I/O error
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 */
	@POST
	@Path("/uploadResource")
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@Operation(summary = "Uploads a new resource of the document")
	@ApiResponses(value = { @ApiResponse(responseCode = "204", description = "successful operation") })
	public void uploadResource(
			@Multipart(value = "docId", required = false) String docId, 
			@Multipart(value = "fileVersion", required = false)	String fileVersion, 
			@Multipart(value = "suffix", required = false) String suffix, 
			@Multipart(value = "content", required = false)	Attachment contentDetail)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException, IOException;

	/**
	 * Restores a deleted document
	 * 
	 * @param docId The document id
	 * @param folderId Id of the folder in which the document must be restored
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws AuthenticationException Invalid credentials
	 */
	public void restore(long docId, long folderId)
			throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Adds a new note for the given document
	 * 
	 * @param docId identifier of the document
	 * @param note the note to add
	 * 
	 * @return the added note
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 * @throws UnexistingResourceException The specified document does not exist
	 */
	public WSNote saveNote(long docId, WSNote note) throws AuthenticationException, PermissionException,
			WebserviceException, PersistenceException, UnexistingResourceException;

	/**
	 * Sends a set of documents as mail attachments
	 * 
	 * @param docIds Set of document identifiers
	 * @param recipients Set of recipients(comma separated)
	 * @param subject The email subject
	 * @param message The email message body
	 * 
	 * @throws IOException I/O error
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws AuthenticationException Invalid credentials
	 * @throws MessagingException Error in the communication with the mail
	 *         server
	 */
	public void sendEmail(List<Long> docIds, String recipients, String subject, String message)
			throws AuthenticationException, WebserviceException, PersistenceException, IOException, MessagingException;

	/**
	 * Puts a password protection to the document
	 * 
	 * @param docId identifier of the document
	 * @param password the new password
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 * @throws UnexistingResourceException The specified document does not exist
	 */
	public void setPassword(long docId, String password) throws AuthenticationException, PermissionException,
			WebserviceException, PersistenceException, UnexistingResourceException;

	/**
	 * Unlocks an existing document with the given identifier.
	 * 
	 * @param docId identifier of the document
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 * @throws UnexistingResourceException The specified document does not exist
	 */
	public void unlock(long docId) throws AuthenticationException, PermissionException, WebserviceException,
			PersistenceException, UnexistingResourceException;

	/**
	 * Unprotects a document that is password protected. If the given password
	 * is right, the document remains unprotected for the duration of the
	 * session
	 * 
	 * @param docId identifier of the document
	 * @param password the password
	 * 
	 * @return was it uprotected?
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws AuthenticationException Invalid session
	 */
	public boolean unprotect(long docId, String password)
			throws PersistenceException, AuthenticationException, WebserviceException;

	/**
	 * Removes the password protection from the document
	 * 
	 * @param docId identifier of the document
	 * @param currentPassword the password
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 * @throws UnexistingResourceException The specified document does not exist
	 */
	public void unsetPassword(long docId, String currentPassword) throws AuthenticationException, PermissionException,
			WebserviceException, PersistenceException, UnexistingResourceException;

	/**
	 * Sets the Access Control List
	 * 
	 * @param docId Document id
	 * @param acl the complete Access Control List
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@PUT
	@Path("/setAccessControlList")
	public void setAccessControlList(@QueryParam("docId")
	long docId, List<WSAccessControlEntry> acl)
			throws PersistenceException, PermissionException, AuthenticationException, WebserviceException;

	/**
	 * Retrieves the access control list
	 * 
	 * @param docId Document id
	 * 
	 * @return 'error' if error occurred, the right objects collection
	 * 
	 * @throws PermissionException The permission has not been granted
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@GET
	@Path("/getAccessControlList")
	public List<WSAccessControlEntry> getAccessControlList(@QueryParam("docId")
	long docId) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Tests if a document is readable.
	 * 
	 * @param docId The document id
	 * 
	 * @return True if the identifier denotes a readable document, otherwise
	 *         false.
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws AuthenticationException Invalid credentials
	 */
	@GET
	@Path("/isRead")
	public boolean isRead(@QueryParam("docId")
	long docId) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Tests if a document is downloadable.
	 * 
	 * @param docId The document id
	 * 
	 * @return True if the identifier denotes a downloadable document, otherwise
	 *         false.
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws AuthenticationException Invalid credentials
	 */
	@GET
	@Path("/isDownload")
	public boolean isDownload(@QueryParam("docId")
	long docId) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Tests if a document is writable
	 * 
	 * @param docId The document id
	 * 
	 * @return True if the identifier denotes a writable document, otherwise
	 *         false
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws AuthenticationException Invalid credentials
	 */
	@GET
	@Path("/isWrite")
	public boolean isWrite(@QueryParam("docId")
	long docId) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Tests if the current user has a specific permission on a document
	 * 
	 * @param docId The document id
	 * @param permission The permission to check (eg: 'read', 'write', ...)
	 * 
	 * @return True if the identifier denotes a granted permission, otherwise
	 *         false
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws AuthenticationException Invalid credentials
	 */
	@GET
	@Path("/isGranted")
	public boolean isGranted(@QueryParam("docId")
	long docId, @QueryParam("permission")
	String permission) throws AuthenticationException, WebserviceException, PersistenceException;
}