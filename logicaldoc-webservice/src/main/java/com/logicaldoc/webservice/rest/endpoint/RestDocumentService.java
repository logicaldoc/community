package com.logicaldoc.webservice.rest.endpoint;

import java.io.IOException;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.Multipart;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Strings;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.parser.ParsingException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.authorization.UnexistingResourceException;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.util.spring.Context;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSAccessControlEntry;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSLink;
import com.logicaldoc.webservice.model.WSNote;
import com.logicaldoc.webservice.model.WSRating;
import com.logicaldoc.webservice.rest.DocumentService;
import com.logicaldoc.webservice.soap.endpoint.SoapDocumentService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Encoding;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.parameters.RequestBody;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.activation.DataHandler;
import jakarta.mail.MessagingException;
import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.DELETE;
import jakarta.ws.rs.FormParam;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.PUT;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.PathParam;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.QueryParam;
import jakarta.ws.rs.WebApplicationException;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.PathSegment;
import jakarta.ws.rs.core.Response;

@Path("/")
@Tag(name = "document")
@Consumes({ MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON })
@Produces({ MediaType.APPLICATION_JSON })
public class RestDocumentService extends SoapDocumentService implements DocumentService {

	private static final Logger log = LoggerFactory.getLogger(RestDocumentService.class);

	/**
	 * Creates a new document using the metadata document object provided as
	 * JSON/XML
	 * 
	 * @param document the document's metadata
	 * @param contentDetail the file content
	 * 
	 * @return the created document
	 */
	@Override
	@POST
	@Path("/create")
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@Produces(MediaType.APPLICATION_JSON)
	@Operation(operationId = "createDocument", summary = "Creates a new document", description = "Creates a new document using the metadata document object provided as JSON/XML")
	@ApiResponses(value = {
			@ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = WSDocument.class))),
			@ApiResponse(responseCode = "401", description = "Authentication / Authorization failed"),
			@ApiResponse(responseCode = "500", description = "Generic error, see the response message") })
	@RequestBody(content = @Content(mediaType = MediaType.MULTIPART_FORM_DATA, schema = @Schema(implementation = CreateDocumentMultipartRequest.class), encoding = @Encoding(name = "file", contentType = "application/octet-stream")))
	public WSDocument create(@Multipart(value = "document", required = true, type = "application/json")
	WSDocument document, @Multipart(value = "content", required = true, type = "application/octet-stream")
	Attachment contentDetail) {

		log.debug("createDocument()");

		String sid = validateSessionREST();

		log.debug("document: {}", document);
		log.debug("contentDetail: {}", contentDetail);

		DataHandler content = contentDetail.getDataHandler();

		try {
			return super.create(sid, document, content);
		} catch (AuthenticationException | PermissionException e) {
			throw new WebApplicationException(e.getMessage(), 401);
		} catch (Exception e) {
			throw new WebApplicationException(e.getMessage(), 500);
		}
	}

	public class CreateDocumentMultipartRequest {
		@Schema(implementation = WSDocument.class, required = true, description = "The document metadata provided as WSDocument object encoded in JSON/XML format")
		public WSDocument document;

		@Schema(type = "string", format = "binary", required = true, description = "File data")
		public Attachment content;
	}

	@Override
	@GET
	@Path("/getDocument")
	@Produces({ MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
	@Operation(summary = "Gets document metadata", description = "Gets the document metadata")
	@ApiResponses(value = {
			@ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = WSDocument.class))),
			@ApiResponse(responseCode = "401", description = "Authentication / Authorization failed"),
			@ApiResponse(responseCode = "500", description = "Generic error, see the response message") })
	public WSDocument getDocument(@QueryParam("docId")
	long docId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();

		try {
			return super.getDocument(sid, docId);
		} catch (AuthenticationException | PermissionException | UnexistingResourceException e) {
			throw new WebApplicationException(e.getMessage(), 401);
		} catch (Exception e) {
			throw new WebApplicationException(e.getMessage(), 500);
		}
	}

	@Override
	@POST
	@Path("/checkout")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Operation(summary = "Checkout a document", description = "Performs the checkout operation on a document. The document status will be changed to checked-out")
	public void checkout(@FormParam("docId")
	long docId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		super.checkout(sid, docId);
	}

	@Override
	@POST
	@Path("/checkin")
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@Operation(summary = "Check-in an existing document", description = "Performs a check-in (commit) operation of new content over an existing document. The document must be in checked-out status")
	@ApiResponses(value = { @ApiResponse(responseCode = "204", description = "Successful operation"),
			@ApiResponse(responseCode = "401", description = "Authentication failed"),
			@ApiResponse(responseCode = "500", description = "Generic error, see the response message") })
	@RequestBody(content = @Content(mediaType = MediaType.MULTIPART_FORM_DATA, schema = @Schema(implementation = CheckinDocumentMultipartRequest.class), encoding = @Encoding(name = "file", contentType = "application/octet-stream")))
	public void checkin(@Multipart(value = "docId", required = true)
	String docId, @Multipart(value = "comment", required = false)
	String comment, @Multipart(value = "release", required = false)
	String releaseStr, @Multipart(value = "filename", required = true)
	String filename, @Multipart(value = "filedata", required = true)
	Attachment filedataDetail) {

		String sid = validateSessionREST();

		try {
			boolean release = false;
			if (!Strings.isNullOrEmpty(releaseStr)) {
				release = Boolean.parseBoolean(releaseStr);
			}

			DataHandler datah = filedataDetail.getDataHandler();

			super.checkin(sid, Long.parseLong(docId), comment, filename, release, datah);
		} catch (Exception t) {
			throw new WebApplicationException(t.getMessage(), 500);
		}
	}

	public class CheckinDocumentMultipartRequest {

		@Schema(type = "integer", required = true, description = "The ID of an existing document to update")
		public String docId;

		@Schema(type = "string", required = false, description = "An optional comment")
		public String comment;

		@Schema(type = "string", required = false, allowableValues = { "true",
				"false" }, description = "Indicates whether to create or not a new major release of")
		public String release;

		@Schema(type = "string", required = true, description = "File name")
		public String filename;

		@Schema(type = "string", format = "binary", required = true, description = "File data")
		public Attachment filedata;
	}

	@Override
	@POST
	@Path("/upload")
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@Operation(summary = "Uploads a document", description = "Creates or updates an existing document, if used in update mode docId must be provided, when used in create mode folderId is required. Returns the ID of the created/updated document. &lt;br/&gt;Example: curl -u admin:admin -H ''Accept: application/json'' -X POST -F folderId=4 -F filename=newDoc.txt -F filedata=@newDoc.txt http://localhost:8080/services/rest/document/upload")
	@ApiResponses(value = {
			@ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Long.class, description = "The ID of the document created or updated"))),
			@ApiResponse(responseCode = "401", description = "Authentication failed"),
			@ApiResponse(responseCode = "500", description = "Generic error, see the response message") })
	@RequestBody(content = @Content(mediaType = MediaType.MULTIPART_FORM_DATA, schema = @Schema(implementation = UploadDocumentMultipartRequest.class), encoding = @Encoding(name = "file", contentType = "application/octet-stream")))
	public Long upload(@Multipart(value = "docId", required = false)
	String docId, @Multipart(value = "folderId", required = false)
	String folderId, @Multipart(value = "release", required = false)
	String release, @Multipart(value = "filename", required = true)
	String filename, @Multipart(value = "language", required = false)
	String language, @Multipart(value = "filedata", required = true)
	Attachment filedataDetail) {

		String sid = validateSessionREST();
		try {
			Long docIdLong = null;
			Long folderIdLong = null;
			boolean releaseBoolean = false;

			if (docId != null) {
				docIdLong = Long.parseLong(docId);
			}
			if (!Strings.isNullOrEmpty(folderId)) {
				folderIdLong = Long.parseLong(folderId);
			}
			if (!Strings.isNullOrEmpty(release)) {
				releaseBoolean = Boolean.parseBoolean(release);
			}
			DataHandler datah = filedataDetail.getDataHandler();

			return super.upload(sid, docIdLong, folderIdLong, releaseBoolean, filename, language, datah);
		} catch (Exception t) {
			throw new WebApplicationException(t.getMessage(), 500);
		}
	}

	public class UploadDocumentMultipartRequest {

		@Schema(type = "integer", required = false, description = "The ID of an existing document to update")
		public String docId;

		@Schema(type = "string", required = false, description = "Folder ID where to place the document")
		public String folderId;

		@Schema(type = "string", required = false, allowableValues = { "true",
				"false" }, description = "Indicates whether to create or not a new major release of an updated document")
		public String release;

		@Schema(type = "string", required = true, description = "File name")
		public String filename;

		@Schema(type = "string", required = false, defaultValue = "en", description = "Language of the document (ISO 639-2)")
		public String language;

		@Schema(type = "string", format = "binary", required = true, description = "File data")
		public Attachment filedata;
	}

	/**
	 * Replace the file of a version
	 *
	 * Replaces the file associated to a given version.
	 *
	 */
	@Override
	@POST
	@Path("/replaceFile")
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@Operation(summary = "Replace the file of a version", description = "Replaces the file associated to a given version.")
	@ApiResponses(value = { @ApiResponse(responseCode = "204", description = "successful operation"),
			@ApiResponse(responseCode = "400", description = "bad request"),
			@ApiResponse(responseCode = "401", description = "Authentication failed"),
			@ApiResponse(responseCode = "500", description = "Generic error, see the response message") })
	@RequestBody(content = @Content(mediaType = MediaType.MULTIPART_FORM_DATA, schema = @Schema(implementation = ReplaceFileMultipartRequest.class), encoding = @Encoding(name = "file", contentType = "application/octet-stream")))
	public void replaceFile(
			@Multipart(value = "docId", required = true) String docId, 
			@Multipart(value = "fileVersion", required = false)	String fileVersion, 
			@Multipart(value = "comment", required = false)	String comment, 
			@Multipart(value = "filedata", required = true)	Attachment filedataDetail) {

		String sid = validateSessionREST();

		try {
			DataHandler datah = filedataDetail.getDataHandler();
			
			Long docIdLong = Long.parseLong(docId);

			super.replaceFile(sid, docIdLong, fileVersion, comment, datah);
		} catch (Exception t) {
			throw new WebApplicationException(t.getMessage(), 500);
		}
	}

	public class ReplaceFileMultipartRequest {

		@Schema(type = "integer", required = true, description = "The ID of an existing document to update")
		public String docId;

		@Schema(type = "string", required = false, description = "The file version")
		public String fileVersion;

		@Schema(type = "string", required = false, description = "Comment")
		public String comment;

		@Schema(type = "string", format = "binary", required = true, description = "File data")
		public Attachment filedata;
	}

	@Override
	@DELETE
	@Path("/delete")
	@Operation(summary = "Deletes a document")
	public void delete(@Parameter(description = "Document ID to delete", required = true)
	@QueryParam("docId")
	long docId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		super.delete(sid, docId);
	}

	@Override
	@GET
	@Path("/list")
	@Produces({ MediaType.APPLICATION_JSON })
	@Operation(summary = "Lists documents by folder", description = "Lists Documents by folder identifier")
	public List<WSDocument> list(@QueryParam("folderId")
	long folderId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.listDocuments(sid, folderId, null);
	}
	
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
	@Override
	@GET
	@Path("/listPaginated")
	@Produces({ MediaType.APPLICATION_JSON })
	public List<WSDocument> listPaginated(@QueryParam("folderId")
	long folderId, @QueryParam("fileName")
	String fileName, @QueryParam("sort")
	String sort, @QueryParam("page")
	Integer page, @QueryParam("max")
	Integer max) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.list(sid, folderId, fileName, sort, page, max);
	}

	@Override
	@GET
	@Path("/listDocuments")
	@Operation(summary = "Lists documents by folder and filename", description = "Lists Documents by folder ID filtering the results by filename")
	public List<WSDocument> listDocuments(@QueryParam("folderId")
	long folderId, @QueryParam("fileName")
	String fileName) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.listDocuments(sid, folderId, fileName);
	}

	@GET
	@Path("/getContent")
	@Produces(MediaType.APPLICATION_OCTET_STREAM)
	@Operation(summary = "Gets the document content", description = "Returns the content of a document using the document ID in input")
	@ApiResponses(value = {
			@ApiResponse(description = "default response", content = @Content(mediaType = "application/octet-stream", schema = @Schema(type = "string", format = "binary"))) })
	public DataHandler getContent(@QueryParam("docId")
	long docId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			IOException {
		String sid = validateSessionREST();
		return super.getContent(sid, docId);
	}

	@GET
	@Path("/getVersionContent")
	@Produces(MediaType.APPLICATION_OCTET_STREAM)
	@Operation(summary = "Gets the document content by version", description = "Returns the content of a document using the document ID and version")
	@ApiResponses(value = {
			@ApiResponse(description = "default response", content = @Content(mediaType = "application/octet-stream", schema = @Schema(type = "string", format = "binary"))) })
	public DataHandler getVersionContent(@QueryParam("docId")
	long docId, @QueryParam("version")
	String version) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			IOException {
		String sid = validateSessionREST();
		return super.getVersionContent(sid, docId, version);
	}

	@DELETE
	@Path("/deleteVersion")
	@Operation(summary = "Delete the version of a document", description = "Deletes the version of a document using the document ID and version."
			+ " You can not delete the latest version of a document. Returns the latest version of the document")
	public String deleteVersion(@QueryParam("docId")
	@Parameter(description = "Document ID", required = true)
	long docId, @QueryParam("version")
	String version) throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.deleteVersion(sid, docId, version);
	}

	@Override
	@PUT
	@Path("/update")
	@Operation(summary = "Updates an existing document", description = "Updates the metadata of an existing document. The ID of the document must be specified in the WSDocument value object. The provided example moves document with ID 1111111 to folder 3435433")
	@ApiResponses(value = { 
			@ApiResponse(responseCode = "204", description = "Successful operation"),
			@ApiResponse(responseCode = "401", description = "Operation failed, authentication error. See system logs"),
			@ApiResponse(responseCode = "406", description = "Operation failed, the server could not produce a response matching the list of acceptable values defined in the request"),
			@ApiResponse(responseCode = "500", description = "Operation failed, there may be a problem with the data provided for the update. Please see system logs")})	
	public void update(
			@Parameter(description = "Document object that needs to be updated", required = true, example = "{ \"id\": 1111111, \"folderId\": 3435433 }")
			WSDocument document) throws AuthenticationException, PermissionException, WebserviceException,
			PersistenceException, UnexistingResourceException {
		String sid = validateSessionREST();
		super.update(sid, document);
	}

	@Override
	@POST
	@Path("/addNote")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Operation(summary = "Adds a new note for the given document")
	public WSNote addNote(@FormParam("docId")
	@Parameter(description = "Document ID", required = true)
	long docId, @FormParam("note")
	@Parameter(description = "Text of the note to add", required = true)
	String note) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			UnexistingResourceException {
		String sid = validateSessionREST();
		return super.addNote(sid, docId, note);
	}

	@Override
	@DELETE
	@Path("/deleteNote")
	@Operation(summary = "Deletes a note")
	public void deleteNote(@QueryParam("noteId")
	@Parameter(description = "ID of the note to delete", required = true)
	long noteId) throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		super.deleteNote(sid, noteId);
	}

	@Override
	@GET
	@Path("/getNotes")
	@Operation(summary = "Gets all the notes of a document")
	public List<WSNote> getNotes(@QueryParam("docId")
	@Parameter(description = "Document ID", required = true)
	long docId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			UnexistingResourceException {
		String sid = validateSessionREST();
		return super.getNotes(sid, docId);
	}

	@Override
	@PUT
	@Path("/rateDocument")
	@Operation(summary = "Add/Update the user's vote for a document")
	public WSRating rateDocument(@QueryParam("docId")
	@Parameter(description = "Document ID", required = true)
	long docId, @QueryParam("vote")
	@Parameter(description = "The user's vote", required = true)
	int vote) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			UnexistingResourceException {
		String sid = validateSessionREST();
		return super.rateDocument(sid, docId, vote);
	}

	@Override
	@GET
	@Path("/getRatings")
	@Operation(summary = "Retrieves the different ratings of a focuments")
	public List<WSRating> getRatings(@QueryParam("docId")
	@Parameter(description = "Document ID", required = true)
	long docId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			UnexistingResourceException {
		String sid = validateSessionREST();
		return super.getRatings(sid, docId);
	}

	@Override
	@PUT
	@Path("/move")
	@Operation(summary = "Moves an existing document with the given identifier")
	public void move(@QueryParam("docId")
	@Parameter(description = "Document ID", required = true)
	long docId, @QueryParam("folderId")
	@Parameter(description = "Target Folder ID", required = true)
	long folderId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			UnexistingResourceException {
		String sid = validateSessionREST();
		super.move(sid, docId, folderId);
	}

	@Override
	@PUT
	@Path("/copy")
	@Operation(summary = "Copies a document into a folder")
	@Produces(MediaType.APPLICATION_JSON)
	@ApiResponses(value = {
			@ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = WSDocument.class))),
			@ApiResponse(responseCode = "401", description = "Authentication failed"),
			@ApiResponse(responseCode = "500", description = "Generic error, see the response message") })
	public WSDocument copy(@QueryParam("docId")
	@Parameter(description = "Document ID", required = true)
	long docId, @QueryParam("folderId")
	@Parameter(description = "Target Folder ID", required = true)
	long folderId, @QueryParam("links")
	@Parameter(description = "If links must be copied too", required = true)
	boolean links, @QueryParam("notes")
	@Parameter(description = "If notes and annotations must be copied too", required = true)
	boolean notes, @QueryParam("security")
	@Parameter(description = "If security settings must be copied too", required = true)
	boolean security) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			IOException {
		String sid = validateSessionREST();
		return super.copy(sid, docId, folderId, links, notes, security);
	}

	@Override
	@PUT
	@Path("/createThumbnail")
	@Operation(summary = "Creates the thumbail of the given document; if the thumbnail was already created, nothing will happen")
	public void createThumbnail(@QueryParam("docId")
	long docId, @QueryParam("fileVersion")
	String fileVersion, @QueryParam("type")
	String type) throws AuthenticationException, WebserviceException, PersistenceException, IOException {
		String sid = validateSessionREST();
		super.createThumbnail(sid, docId, fileVersion, type);
	}

	@Override
	@PUT
	@Path("/createPdf")
	@Operation(summary = "Creates the PDF conversion of the given document; if the PDF conversion was already created, nothing will happen")
	public void createPdf(@QueryParam("docId")
	long docId, @QueryParam("fileVersion")
	String fileVersion) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			IOException {
		String sid = validateSessionREST();
		super.createPdf(sid, docId, fileVersion);
	}

	@Override
	@PUT
	@Path("/promoteVersion")
	@Operation(summary = "Promotes an old version to the current default one")
	public void promoteVersion(@QueryParam("docId")
	@Parameter(description = "Document ID", required = true)
	long docId, @QueryParam("version")
	String version) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			IOException, UnexistingResourceException {
		String sid = validateSessionREST();
		super.promoteVersion(sid, docId, version);
	}

	@Override
	@PUT
	@Path("/rename")
	@Operation(summary = "Renames the title of an existing document with the given identifier")
	public void rename(@QueryParam("docId")
	@Parameter(description = "Document ID", required = true)
	long docId, @QueryParam("name")
	@Parameter(description = "The new document filename", required = true)
	String name) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			UnexistingResourceException {
		String sid = validateSessionREST();
		super.rename(sid, docId, name);
	}

	@Override
	@GET
	@Path("/getVersions")
	@Operation(summary = "Gets the versions", description = "Gets the version history of an existing document with the given identifier")
	public List<WSDocument> getVersions(@QueryParam("docId")
	@Parameter(description = "Document ID", required = true)
	long docId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			UnexistingResourceException {
		String sid = validateSessionREST();
		return super.getVersions(sid, docId);
	}

	@Override
	@POST
	@Path("/createAlias")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Operation(summary = "Creates a new document alias", description = "Creates a new document alias for the given document inside a specified folder")
	public WSDocument createAlias(@FormParam("docId")
	@Parameter(description = "Source document ID", required = true)
	long docId, @FormParam("folderId")
	@Parameter(description = "Target folder ID", required = true)
	long folderId, @FormParam("type")
	@Parameter(description = "Type of the alias (use 'pdf' to create an alias to the PDF conversion)", required = true)
	String type) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.createAlias(sid, docId, folderId, type);
	}

	@Override
	@POST
	@Path("/createDownloadTicket")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Operation(summary = "Creates a new download ticket", description = "Creates a new download ticket to the original document or it's PDF conversion")
	public String createDownloadTicket(@FormParam("docId")
	@Parameter(description = "Source document ID", required = true)
	long docId, @FormParam("suffix")
	@Parameter(description = "can be null or 'conversion.pdf'")
	String suffix, @FormParam("expireHours")
	@Parameter(description = "expiration time expressed in hours")
	Integer expireHours, @FormParam("expireDate")
	@Parameter(description = "exact expiration date expressed in the format yyyy-MM-dd")
	String expireDate, @FormParam("maxDownloads")
	@Parameter(description = "maximum number of allowed downloads")
	Integer maxDownloads, @Parameter(description = "Optional password")
	String password)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		String sid = validateSessionREST();
		return super.createDownloadTicket(sid, docId, suffix, expireHours, expireDate, maxDownloads, password);
	}

	@Override
	@POST
	@Path("/createViewTicket")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Operation(summary = "Creates a new view ticket", description = "Creates a new view ticket to the original document or it's PDF conversion")
	public String createViewTicket(@FormParam("docId")
	@Parameter(description = "Source document ID", required = true)
	long docId, @FormParam("suffix")
	@Parameter(description = "can be null or 'conversion.pdf'")
	String suffix, @FormParam("expireHours")
	@Parameter(description = "expiration time expressed in hours")
	Integer expireHours, @FormParam("expireDate")
	@Parameter(description = "exact expiration date expressed in the format yyyy-MM-dd")
	String expireDate, @FormParam("maxDownloads")
	@Parameter(description = "maximum number of downloads allowed")
	Integer maxDownloads, @FormParam("maxViews")
	@Parameter(description = "maximum number of downloads views")
	Integer maxViews, @Parameter(description = "Optional password")
	String password) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		String sid = validateSessionREST();
		return super.createViewTicket(sid, docId, suffix, expireHours, expireDate, maxDownloads, maxViews, password);
	}

	@Override
	@DELETE
	@Path("/deleteLink")
	@Operation(summary = "Removes an existing link")
	public void deleteLink(@Parameter(description = "ID of the link", required = true)
	@QueryParam("id")
	long id) throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		super.deleteLink(sid, id);
	}

	@Override
	@GET
	@Path("/getAliases")
	@Operation(summary = "Gets the aliases", description = "Gets the aliases of the given document; returns an array of WSDocument that are aliases")
	public List<WSDocument> getAliases(@Parameter(description = "The document ID", required = true)
	@QueryParam("docId")
	long docId) throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.getAliases(sid, docId);
	}

	@Override
	@GET
	@Path("/getDocumentByCustomId")
	@Operation(summary = "Gets document metadata by custom ID", description = "Gets document metadata of an existing document with the given custom identifier")
	public WSDocument getDocumentByCustomId(@Parameter(description = "The custom ID", required = true)
	@QueryParam("customId")
	String customId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.getDocumentByCustomId(sid, customId);
	}

	@Override
	@GET
	@Path("/getDocuments")
	@Operation(summary = "Gets the metadata of a collection of document", description = "Gets document metadata of a collection of existing documents with the given identifiers; returns an array of WSDocument")
	public List<WSDocument> getDocuments(@QueryParam("docIds")
	@Parameter(description = "List of document IDs", required = true)
	List<Long> docIds) throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.getDocuments(sid, docIds);
	}

	@Override
	@GET
	@Path("/getExtractedText")
	@Produces({ MediaType.TEXT_PLAIN })
	@Operation(summary = "Gets the extracted text of a document", description = "Gets the document's text stored in the full-text index")
	public String getExtractedText(@QueryParam("docId")
	@Parameter(description = "The document ID", required = true)
	long docId) throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.getExtractedText(sid, docId);
	}

	@Override
	@GET
	@Path("/getRecentDocuments")
	@Operation(summary = "Gets the last modified documents", description = "Lists of last modified documents of the current session")
	public List<WSDocument> getRecentDocuments(@QueryParam("maxHits")
	@Parameter(description = "Maximum number of returned records", required = true)
	Integer maxHits) throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.getRecentDocuments(sid, maxHits);
	}

	@Override
	@GET
	@Path("/getLinks")
	@Operation(summary = "Gets the links of a document", description = "Gets all the links of a specific document; returns an array of links")
	public List<WSLink> getLinks(@QueryParam("docId")
	@Parameter(description = "The document ID", required = true)
	long docId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.getLinks(sid, docId);
	}

	@Override
	@GET
	@Path("/getResource")
	@Produces(MediaType.APPLICATION_OCTET_STREAM)
	@Operation(summary = "Gets the content of a resource", description = "Gets the content of a resource associated to the given document; returns the raw content of the file")
	@ApiResponses(value = {
			@ApiResponse(description = "default response", content = @Content(mediaType = "application/octet-stream", schema = @Schema(type = "string", format = "binary"))) })
	public DataHandler getResource(@QueryParam("docId")
	@Parameter(description = "The document ID", required = true)
	long docId, @QueryParam("fileVersion")
	@Parameter(description = "The file version to retrieve")
	String fileVersion, @QueryParam("suffix")
	@Parameter(description = "suffix specification(it cannot be empty, use 'conversion.pdf' to get the PDF conversion)")
	String suffix) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			IOException {
		String sid = validateSessionREST();
		return super.getResource(sid, docId, fileVersion, suffix);
	}

	@Override
	@POST
	@Path("/link")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Operation(summary = "Creates a link between two documents", description = "Creates a new link between two documents; returns the created link object")
	public WSLink link(@FormParam("doc1")
	@Parameter(description = "ID of document 1", required = true)
	long doc1, @FormParam("doc2")
	@Parameter(description = "ID of document 2", required = true)
	long doc2, @FormParam("type")
	@Parameter(description = "type of the link (use 'pdf' to point to the pdf conversion)")
	String type) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			UnexistingResourceException {
		String sid = validateSessionREST();
		return super.link(sid, doc1, doc2, type);
	}

	@Override
	@PUT
	@Path("/lock")
	@Operation(summary = "Locks a document", description = "Locks an existing document with the given identifier")
	public void lock(@QueryParam("docId")
	@Parameter(description = "Document ID", required = true)
	long docId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			UnexistingResourceException {
		String sid = validateSessionREST();
		super.lock(sid, docId);
	}

	@Override
	@POST
	@Path("/reindex")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Operation(summary = "Re-indexes a document", description = "re-indexes(or indexes from scratch) a document")
	public void reindex(@FormParam("doc1")
	@Parameter(description = "Document ID", required = true)
	long docId, @FormParam("content")
	@Parameter(description = "Document ID")
	String content) throws AuthenticationException, ParsingException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		super.reindex(sid, docId, content);
	}

	@Override
	@POST
	@Path("/uploadResource")
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@Operation(summary = "Uploads a new resource of the document", description = "Uploads a new resource attached to the given document. If the resource already exists it is overwritten")
	@ApiResponses(value = { @ApiResponse(responseCode = "204", description = "successful operation") })
	@RequestBody(content = @Content(mediaType = MediaType.MULTIPART_FORM_DATA, schema = @Schema(implementation = UploadResourceMultipartRequest.class), encoding = @Encoding(name = "file", contentType = "application/octet-stream")))
	public void uploadResource(
			@Multipart(value = "docId", required = true) String docId, 
			@Multipart(value = "fileVersion", required = false) String fileVersion, 
			@Multipart(value = "suffix", required = true) String suffix, 
			@Multipart(value = "content", required = true) Attachment contentDetail) 
					throws AuthenticationException, PermissionException, WebserviceException, PersistenceException, IOException {

		String sid = validateSessionREST();

		DataHandler datah = contentDetail.getDataHandler();

		Long docIdLong = Long.parseLong(docId);

		super.uploadResource(sid, docIdLong, fileVersion, suffix, datah);
	}

	public class UploadResourceMultipartRequest {

		@Schema(type = "integer", required = true, description = "The document ID")
		public String docId;

		@Schema(type = "string", required = false, description = "the specific file version")
		public String fileVersion;

		@Schema(type = "string", required = false, description = "suffix specification(it cannot be empty, use 'conversion.pdf' to put the PDF conversion)")
		public String suffix;

		@Schema(type = "string", format = "binary", required = true, description = "raw content of the file")
		public Attachment content;
	}

	@Override
	@PUT
	@Path("/restore")
	@Operation(summary = "Restores a deleted document")
	public void restore(@QueryParam("docId")
	@Parameter(description = "Document ID", required = true)
	long docId, @QueryParam("folderId")
	@Parameter(description = "Folder ID (target)", required = true)
	long folderId) throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		super.restore(sid, docId, folderId);
	}

	@Override
	@POST
	@Path("/saveNote")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Operation(summary = "Adds a new note", description = "Adds/modifies a note for the given document")
	public WSNote saveNote(@FormParam("docId")
	@Parameter(description = "Document ID", required = true)
	long docId, @FormParam("note")
	@Parameter(description = "the WSNote representation as json string", required = true)
	WSNote note) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			UnexistingResourceException {
		String sid = validateSessionREST();
		return super.saveNote(sid, docId, note);
	}

	@Override
	@POST
	@Path("/sendEmail")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Operation(summary = "Sends documents by email", description = "Sends a set of documents as mail attachments")
	public void sendEmail(@FormParam("docIds")
	@Parameter(description = "Document IDs", required = true)
	List<Long> docIds, @FormParam("recipients")
	@Parameter(description = "Set of recipients(comma separated)", required = true)
	String recipients, @FormParam("subject")
	@Parameter(description = "The email subject")
	String subject, @FormParam("message")
	@Parameter(description = "The email message body")
	String message)
			throws AuthenticationException, WebserviceException, PersistenceException, IOException, MessagingException {
		String sid = validateSessionREST();
		super.sendEmail(sid, docIds, recipients, subject, message);
	}

	@Override
	@POST
	@Path("/setPassword")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Operation(summary = "Password protect a document", description = "Protects with a password the given document")
	public void setPassword(@FormParam("docId")
	@Parameter(description = "Document ID", required = true)
	long docId, @FormParam("password")
	@Parameter(description = "A password", required = true)
	String password) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			UnexistingResourceException {
		String sid = validateSessionREST();
		super.setPassword(sid, docId, password);
	}

	@Override
	@PUT
	@Path("/unlock")
	@Operation(summary = "Unlocks the document", description = "Unlocks an existing document with the given identifier")
	public void unlock(@QueryParam("docId")
	@Parameter(description = "Document ID", required = true)
	long docId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			UnexistingResourceException {
		String sid = validateSessionREST();
		super.unlock(sid, docId);
	}

	@Override
	@POST
	@Path("/unsetPassword")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Operation(summary = "Removes password protection", description = "Removes the password protection from the document")
	public void unsetPassword(@FormParam("docId")
	@Parameter(description = "Document ID", required = true)
	long docId, @FormParam("currentPassword")
	@Parameter(description = "A password", required = true)
	String currentPassword) throws AuthenticationException, PermissionException, WebserviceException,
			PersistenceException, UnexistingResourceException {
		String sid = validateSessionREST();
		super.unsetPassword(sid, docId, currentPassword);
	}

	@Override
	@POST
	@Path("/unprotect")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Operation(summary = "Temporarily removes password protection", description = "Unprotect a document that is password protected. If the given password is right, the document remains unprotected for the duration of the session")
	public boolean unprotect(@FormParam("docId")
	@Parameter(description = "Document ID", required = true)
	long docId, @FormParam("password")
	@Parameter(description = "A password", required = true)
	String password) throws PersistenceException, AuthenticationException, WebserviceException {
		String sid = validateSessionREST();
		return super.unprotect(sid, docId, password);
	}

	@Override
	@GET
	@Path("/thumbnail/{type}/{docpath:.*}")
	@Produces("image/png")
	@ApiResponses(value = {
			@ApiResponse(description = "default response", content = @Content(mediaType = "image/png", schema = @Schema(type = "string", format = "binary"))) })
	public Response getThumbnail(@PathParam("type")
	String type, @PathParam("docpath")
	String docPath, @PathParam("docpath")
	List<PathSegment> docPathList) throws AuthenticationException, WebserviceException, PersistenceException,
			IOException, PermissionException {
		String sid = validateSessionREST();

		String myPath = "/" + docPath;

		User user = validateSession(sid);
		DocumentDAO docDao = Context.get(DocumentDAO.class);
		Document doc = docDao.findByPath(myPath, user.getTenantId());

		if (!type.toLowerCase().endsWith(".png"))
			type += ".png";

		DataHandler dh = null;
		try {
			dh = super.getResource(sid, doc.getId(), doc.getFileVersion(), type);			
		} catch (Exception e) {
			super.createThumbnail(sid, doc.getId(), doc.getFileVersion(), type);
			dh = super.getResource(sid, doc.getId(), doc.getFileVersion(), type);
		}
		
		Response resp = Response.ok(dh.getInputStream()).build();        
        resp.getHeaders().add("Cache-Control", "max-age=86400, must-revalidate");
        resp.getHeaders().add("Pragma", "no-cache");   
        resp.getHeaders().add("Content-Type", dh.getContentType());
        
        Calendar cal = Calendar.getInstance();
        cal.add(Calendar.HOUR, 24);
        Date expdate = cal.getTime();
        ZonedDateTime date = ZonedDateTime.ofInstant(expdate.toInstant(), ZoneId.systemDefault());
            
        resp.getHeaders().add("Expires", date.format(DateTimeFormatter.RFC_1123_DATE_TIME));      
        
		return resp;
	}

	@Override
	@GET
	@Path("/isRead")
	@Operation(operationId = "isReadDocument", summary = "Tests if a document is readable")
	public boolean isRead(@QueryParam("docId")
	long docId) throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.isRead(sid, docId);
	}

	@Override
	@GET
	@Path("/isDownload")
	@Operation(operationId = "isDownloadDocument", summary = "Tests if a document is downloadable")
	public boolean isDownload(@QueryParam("docId")
	long docId) throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.isDownload(sid, docId);
	}

	@Override
	@GET
	@Path("/isWrite")
	@Operation(operationId = "isWriteDocument", summary = "Tests if a document is writeable")
	public boolean isWrite(@QueryParam("docId")
	long docId) throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.isWrite(sid, docId);
	}

	@Override
	@GET
	@Path("/isGranted")
	@Operation(summary = "Tests user permission on a document", description = "Tests if the current user has a specific permission on a document")
	public boolean isGranted(@Parameter(description = "Document identifier (ID)", required = true)
	@QueryParam("docId")
	long docId, @Parameter(description = "the permissions' integer representation", required = true)
	@QueryParam("permission")
	String permission) throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.isGranted(sid, docId, permission);
	}

	@Override
	@PUT
	@Path("/setAccessControlList")
	@Operation(operationId = "setAccessControlList_Document", summary = "Assigns the complete Access Control List")
	public void setAccessControlList(@QueryParam("docId")
	long docId, List<WSAccessControlEntry> acl)
			throws PersistenceException, PermissionException, AuthenticationException, WebserviceException {
		String sid = validateSessionREST();
		super.setAccessControlList(sid, docId, acl);
	}

	@Override
	@GET
	@Path("/getAccessControlList")
	@Operation(operationId = "getAccessControlList_Document", summary = "Retrieves the access control list")
	public List<WSAccessControlEntry> getAccessControlList(@QueryParam("docId")
	long docId) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		String sid = validateSessionREST();
		return super.getAccessControlList(sid, docId);
	}
}