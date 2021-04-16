package com.logicaldoc.webservice.rest;

import java.util.List;

import javax.activation.DataHandler;
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

import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.Multipart;

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
    
    @POST
    @Path("/create")
    @Consumes(MediaType.MULTIPART_FORM_DATA)
    @Produces(MediaType.APPLICATION_JSON)
    @Operation(summary = "Creates a new document")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = WSDocument.class))),
        @ApiResponse(responseCode = "401", description = "Authentication failed"),
        @ApiResponse(responseCode = "500", description = "Generic error, see the response message") })
    public WSDocument create(
    		@Multipart(value = "document", required = true, type = "application/json") WSDocument document,  
    		@Multipart(value = "content" , required = true, type = "application/octet-stream") Attachment contentDetail) throws Exception;    

	@GET
	@Path("/getDocument")
	@Produces({ MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
	WSDocument getDocument(@QueryParam("docId") long docId) throws Exception;

	@POST
	@Path("/checkout")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	void checkout(@FormParam("docId") long docId) throws Exception;
	
	/*
	@POST
	@Path("/checkin")
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@Produces({ MediaType.TEXT_PLAIN, MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
	Response checkin(List<Attachment> attachments) throws Exception; */
	
	/**
     * Check-in an existing document
     *
     * Performs a check-in (commit) operation of new content over an existing document. The document must be in checked-out status
     *
     */
    @POST
    @Path("/checkin")
    @Consumes(MediaType.MULTIPART_FORM_DATA)
    @Operation(summary = "Check-in an existing document", description = "Performs a check-in (commit) operation of new content over an existing document. The document must be in checked-out status")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "204", description = "Successful operation"),	
        @ApiResponse(responseCode = "401", description = "Authentication failed"),
        @ApiResponse(responseCode = "500", description = "Generic error, see the response message") })
    public void checkin(@Multipart(value = "docId", required = true) Integer docId, @Multipart(value = "comment", required = false)  String comment, @Multipart(value = "release", required = false)  String release, @Multipart(value = "filename", required = true)  String filename,  @Multipart(value = "filedata" , required = true) Attachment filedataDetail)  throws Exception;        

    /*
	@POST
	@Path("/replaceFile")
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@Produces({ MediaType.TEXT_PLAIN, MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
	Response replaceFile(List<Attachment> attachments) throws Exception;
	*/
    
    /**
     * Replace the file of a version
     *
     * Replaces the file associated to a given version.
     *
     */
    @POST
    @Path("/replaceFile")
    @Consumes(MediaType.MULTIPART_FORM_DATA)
    @Operation(summary = "Replace the file of a version", description = "Replaces the file associated to a given version.")
    @ApiResponses(value = { 
        	@ApiResponse(responseCode = "204", description = "successful operation"),
        	@ApiResponse(responseCode = "400", description = "bad request"),       		
        @ApiResponse(responseCode = "401", description = "Authentication failed"),
        @ApiResponse(responseCode = "500", description = "Generic error, see the response message") })
    public void replaceFile(@Multipart(value = "docId", required = false)  Integer docId, @Multipart(value = "fileVersion", required = false)  String fileVersion, @Multipart(value = "comment", required = false)  String comment,  @Multipart(value = "filedata" , required = false) Attachment filedataDetail)  throws Exception;    
	
/*	
	@POST
	@Path("/upload")
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@Produces({ MediaType.TEXT_PLAIN, MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
	Response upload(List<Attachment> attachments) throws Exception;
*/
	
    /**
     * Uploads a document
     *
     * Creates or updates an existing document, if used in update mode docId must be provided, when used in create mode folderId is required. Returns the ID of the created/updated document. &lt;br/&gt;Example: curl -u admin:admin -H &#x27;&#x27;Accept: application/json&#x27;&#x27; -X POST -F folderId&#x3D;4 -F filename&#x3D;newDoc.txt -F filedata&#x3D;@newDoc.txt http://localhost:8080/services/rest/document/upload
     *
     */
    @POST
    @Path("/upload")
    @Consumes(MediaType.MULTIPART_FORM_DATA)
    @Operation(summary = "Uploads a document", description = "Creates or updates an existing document, if used in update mode docId must be provided, when used in create mode folderId is required. Returns the ID of the created/updated document. &lt;br/&gt;Example: curl -u admin:admin -H ''Accept: application/json'' -X POST -F folderId=4 -F filename=newDoc.txt -F filedata=@newDoc.txt http://localhost:8080/services/rest/document/upload")
//	@ApiImplicitParams({
//	@ApiImplicitParam(name = "docId", value = "The ID of an existing document to update", required = false, dataType = "integer", paramType = "form"),
//	@ApiImplicitParam(name = "folderId", value = "Folder ID where to place the document", required = false, dataType = "string", paramType = "form"),
//	@ApiImplicitParam(name = "release", value = "Indicates whether to create or not a new major release of an updated document", required = false, dataType = "string", paramType = "form", allowableValues = "true, false"),
//	@ApiImplicitParam(name = "filename", value = "File name", required = true, dataType = "string", paramType = "form"),
//	@ApiImplicitParam(name = "language", value = "Language of the document (ISO 639-2)", required = false, dataType = "string", paramType = "form", defaultValue = "en"),
//	@ApiImplicitParam(name = "filedata", value = "File data", required = true, dataType = "file", paramType = "form") })    
    @ApiResponses(value = { 
   		@ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Long.class, description = "The ID of the document created or updated"))),	
        @ApiResponse(responseCode = "401", description = "Authentication failed"),
        @ApiResponse(responseCode = "500", description = "Generic error, see the response message") })
    public Long upload(
    		@Multipart(value = "docId", required = false) Integer docId, @Multipart(value = "folderId", required = false) String folderId, 
    		@Multipart(value = "release", required = false)  String release, @Multipart(value = "filename", required = true)  String filename, 
    		@Multipart(value = "language", required = false)  String language,  @Multipart(value = "filedata" , required = true) Attachment filedataDetail) throws Exception;
	

	@DELETE
	@Path("/delete")
	void delete(@QueryParam("docId") long docId) throws Exception;

	@GET
	@Path("/list")
	@Produces({ MediaType.APPLICATION_JSON })
	WSDocument[] list(@QueryParam("folderId") long folderId) throws Exception;

	@GET
	@Path("/listDocuments")
	WSDocument[] listDocuments(@QueryParam("folderId") long folderId, @QueryParam("fileName") String fileName)
			throws Exception;

	/**
	 * Updates an existing document with the value object containing the
	 * document's metadata.
	 * 
	 * @param document the document to update
	 * 
	 * @throws Exception error in the server application
	 */
	@PUT
	@Path("/update")
	void update(WSDocument document) throws Exception;

	@GET
	@Path("/getContent")
	@Produces(MediaType.APPLICATION_OCTET_STREAM)
	DataHandler getContent(@QueryParam("docId") long docId) throws Exception;

	@GET
	@Path("/getVersionContent")
	@Produces(MediaType.APPLICATION_OCTET_STREAM)
	DataHandler getVersionContent(@QueryParam("docId") long docId, @QueryParam("version") String version)
			throws Exception;

	/**
	 * Adds a new note for the given document
	 * 
	 * @param docId identifier of the document
	 * @param note the note to add
	 * 
	 * @return the added note
	 * 
	 * @throws Exception error in the server application
	 */
	@POST
	@Path("/addNote")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	public WSNote addNote(@FormParam("docId") long docId, @FormParam("note") String note) throws Exception;

	/**
	 * Deletes a new note by note identifier
	 * 
	 * @param noteId identifier of the note
	 * 
	 * @throws Exception error in the server application
	 */
	@DELETE
	@Path("/deleteNote")
	public void deleteNote(@QueryParam("noteId") long noteId) throws Exception;

	/**
	 * Gets the notes for the given document
	 * 
	 * @param docId identifier of the document
	 *
	 * @return array of notes
	 * 
	 * @throws Exception error in the server application
	 */
	@GET
	@Path("/getNotes")
	public WSNote[] getNotes(@QueryParam("docId") long docId) throws Exception;

	/**
	 * Puts a new rating on the given document
	 * 
	 * @param docId identifier of the document
	 * @param vote the vote
	 * 
	 * @return the rating
	 * 
	 * @throws Exception error in the server application
	 */
	@PUT
	@Path("/rateDocument")
	public WSRating rateDocument(@QueryParam("docId") long docId, @QueryParam("vote") int vote) throws Exception;

	/**
	 * Gets all the ratings of the given document
	 * 
	 * @param docId identifier of the document
	 * 
	 * @return the ratings
	 * 
	 * @throws Exception error in the server application
	 */
	@GET
	@Path("/getRatings")
	public WSRating[] getRatings(@QueryParam("docId") long docId) throws Exception;

	/**
	 * Deletes a version by document identifier and version ID. You can not
	 * delete the latest version of a document
	 * 
	 * @param docId identifier of the document
	 * @param version the document's version
	 * 
	 * @return the latest version of the document
	 * 
	 * @throws Exception error in the server application
	 */
	@DELETE
	@Path("/deleteVersion")
	public String deleteVersion(@QueryParam("docId") long docId, @QueryParam("version") String version)
			throws Exception;

	/**
	 * Moves an existing document with the given identifier
	 * 
	 * @param docId The document id
	 * @param folderId Identifier of the new document's folder
	 * 
	 * @throws Exception error in the server application
	 */
	@PUT
	@Path("/move")
	public void move(@QueryParam("docId") long docId, @QueryParam("folderId") long folderId) throws Exception;

	/**
	 * Creates the thumbail of the given document; if the thumbnail was already
	 * created, nothing will happen
	 * 
	 * @param docId The document id
	 * @param fileVersion The specific file version(it can be empty)
	 * @param type The thumbnail type(eg: thumbnail, tile, mobile)
	 * 
	 * @throws Exception error in the server application
	 */
	@PUT
	@Path("/createThumbnail")
	public void createThumbnail(@QueryParam("docId") long docId, @QueryParam("fileVersion") String fileVersion, @QueryParam("type") String type)
			throws Exception;

	// @Produces(MediaType.APPLICATION_OCTET_STREAM)

	@GET
	@Path("/thumbnail/{type}/{docpath:.*}")
	@Produces("image/jpeg")
	public DataHandler getThumbnail(@PathParam("type") String type,
			@PathParam("docpath") String docPath, @PathParam("docpath") List<PathSegment> docPathList) throws Exception;

	/**
	 * Creates the PDF conversion of the given document; if the PDF conversion
	 * was already created, nothing will happen
	 * 
	 * @param docId The document id
	 * @param fileVersion The specific file version(it can be empty)
	 * 
	 * @throws Exception error in the server application
	 */
	@PUT
	@Path("/createPdf")
	public void createPdf(@QueryParam("docId") long docId, @QueryParam("fileVersion") String fileVersion)
			throws Exception;

	/**
	 * Promotes an old version to the current default one. If you promote a
	 * prior version, what it does is make it the default version again.
	 * (regardless of there being many versions).
	 * 
	 * @param docId the document to be updated
	 * @param version the version
	 * 
	 * @throws Exception if an error occurs, this exception is thrown
	 */
	@PUT
	@Path("/promoteVersion")
	public void promoteVersion(@QueryParam("docId") long docId, @QueryParam("version") String version) throws Exception;

	/**
	 * Renames the title of an existing document with the given identifier.
	 * 
	 * @param docId The document id
	 * @param name The new document filename
	 * 
	 * @throws Exception error in the server application
	 */
	@PUT
	@Path("/rename")
	public void rename(@QueryParam("docId") long docId, @QueryParam("name") String name) throws Exception;

	/**
	 * Gets the version history of an existing document with the given
	 * identifier
	 * 
	 * @param docId The document id
	 * 
	 * @return Array of versions
	 * 
	 * @throws Exception error in the server application
	 */
	@GET
	@Path("/getVersions")
	public WSDocument[] getVersions(@QueryParam("docId") long docId) throws Exception;

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
	 * @throws Exception error in the server application
	 */
	@POST
	@Path("/createAlias")
	public WSDocument createAlias(long docId, long folderId, String type) throws Exception;

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
	 * @throws Exception error in the server application
	 */
	@POST
	@Path("/createDownloadTicket")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	public String createDownloadTicket(long docId, String suffix, Integer expireHours, String expireDate,
			Integer maxDownloads) throws Exception;

	/**
	 * Removes an existing link
	 * 
	 * @param id ID of the link
	 * 
	 * @throws Exception error in the server application
	 */
	@DELETE
	@Path("/deleteLink")
	public void deleteLink(long id) throws Exception;

	/**
	 * Gets the aliases of the given document
	 * 
	 * @param docId The master document ID
	 * @return Arrays of aliases
	 * 
	 * @throws Exception error in the server application
	 */
	@GET
	@Path("/getAliases")
	public WSDocument[] getAliases(long docId) throws Exception;

	/**
	 * Gets document metadata of an existing document with the given custom
	 * identifier
	 * 
	 * @param customId The custom id
	 * 
	 * @return A value object containing the document's metadata
	 * 
	 * @throws Exception error in the server application
	 */
	@GET
	@Path("/getDocumentByCustomId")
	public WSDocument getDocumentByCustomId(String customId) throws Exception;

	/**
	 * Gets document metadata of a collection of existing documents with the
	 * given identifiers
	 * 
	 * @param docIds identifiers of the documents
	 * 
	 * @return the list of documents
	 * 
	 * @throws Exception error in the server application
	 */
	@GET
	@Path("/getDocuments")
	public WSDocument[] getDocuments(Long[] docIds) throws Exception;

	/**
	 * Gets the document's text stored in the full-text index
	 * 
	 * @param docId The document id
	 * 
	 * @return The requested document's text
	 * 
	 * @throws Exception error in the server application
	 */
	@GET
	@Path("/getExtractedText")
	@Produces({ MediaType.TEXT_PLAIN })
	public String getExtractedText(long docId) throws Exception;

	/**
	 * Lists of last modified documents of the current session
	 * 
	 * @param maxHits Maximum number of returned records
	 * 
	 * @return Array of documents
	 * 
	 * @throws Exception error in the server application
	 */
	@GET
	@Path("/getRecentDocuments")
	public WSDocument[] getRecentDocuments(Integer maxHits) throws Exception;

	/**
	 * Gets all the links of a specific document
	 * 
	 * @param docId ID of the document
	 * 
	 * @return The new links of the document
	 * 
	 * @throws Exception error in the server application
	 */
	@GET
	@Path("/getLinks")
	public WSLink[] getLinks(long docId) throws Exception;

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
	 * @throws Exception error in the server application
	 */
	@GET
	@Path("/getResource")
	public DataHandler getResource(long docId, String fileVersion, String suffix) throws Exception;

	/**
	 * Tests if a document is readable
	 * 
	 * @param docId The document id
	 * 
	 * @return True if the identifier denotes a document, otherwise false
	 * 
	 * @throws Exception error in the server application
	 */
	public boolean isReadable(long docId) throws Exception;

	/**
	 * Creates a new link between two documents.
	 * 
	 * @param doc1 ID of document 1
	 * @param doc2 ID of document 2
	 * @param type The link type(it can be empty)
	 * 
	 * @return the new link
	 * 
	 * @throws Exception error in the server application
	 */
	public WSLink link(long doc1, long doc2, String type) throws Exception;

	/**
	 * Locks an existing document with the given identifier.
	 * 
	 * @param docId The document id
	 * 
	 * @throws Exception error in the server application
	 */
	public void lock(long docId) throws Exception;

	/**
	 * Re-indexes(or indexes from scratch) a document
	 * 
	 * @param docId The document id
	 * @param content The content to be used (if null the file is parsed)
	 * 
	 * @throws Exception error in the server application
	 */
	public void reindex(long docId, String content) throws Exception;

	/**
	 * Uploads a new resource attached to the given document. If the resource
	 * already exists it is overwritten.
	 * 
	 * @param attachments the attachments to upload
	 * 
	 * @throws Exception error in the server application

	@Consumes(MediaType.MULTIPART_FORM_DATA)
	public void uploadResource(List<Attachment> attachments) throws Exception;
		 */
	
    /**
     * Uploads a new resource of the document
     *
     * Uploads a new resource attached to the given document. If the resource already exists it is overwritten
     *
     */
    @POST
    @Path("/uploadResource")
    @Consumes(MediaType.MULTIPART_FORM_DATA)
    @Operation(summary = "Uploads a new resource of the document")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "204", description = "successful operation") })
    public void uploadResource(@Multipart(value = "docId", required = false)  Integer docId, @Multipart(value = "fileVersion", required = false)  String fileVersion, @Multipart(value = "suffix", required = false)  String suffix,  @Multipart(value = "content" , required = false) Attachment contentDetail)  throws Exception;	

	/**
	 * Restores a deleted document
	 * 
	 * @param docId The document id
	 * @param folderId Id of the folder in which the document must be restored
	 * 
	 * @throws Exception error in the server application
	 */
	public void restore(long docId, long folderId) throws Exception;

	/**
	 * Adds a new note for the given document
	 * 
	 * @param docId identifier of the document
	 * @param note the note to add
	 * 
	 * @return the added note
	 * 
	 * @throws Exception error in the server application
	 */
	public WSNote saveNote(long docId, WSNote note) throws Exception;

	/**
	 * Sends a set of documents as mail attachments
	 * 
	 * @param docIds Set of document identifiers
	 * @param recipients Set of recipients(comma separated)
	 * @param subject The email subject
	 * @param message The email message body
	 * 
	 * @throws Exception error in the server application
	 */
	public void sendEmail(Long[] docIds, String recipients, String subject, String message) throws Exception;

	/**
	 * Puts a password protection to the document
	 * 
	 * @param docId identifier of the document
	 * @param password the new password
	 * 
	 * @throws Exception error in the server application
	 */
	public void setPassword(long docId, String password) throws Exception;

	/**
	 * Unlocks an existing document with the given identifier.
	 * 
	 * @param docId identifier of the document
	 * 
	 * @throws Exception error in the server application
	 */
	public void unlock(long docId) throws Exception;

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
	 * @throws Exception error in the server application
	 */
	public boolean unprotect(long docId, String password) throws Exception;

	/**
	 * Removes the password protection from the document
	 * 
	 * @param docId identifier of the document
	 * @param currentPassword the password
	 * 
	 * @throws Exception error in the server application
	 */
	public void unsetPassword(long docId, String currentPassword) throws Exception;

}