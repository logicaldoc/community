package com.logicaldoc.webservice.rest.endpoint;

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
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.PathSegment;

import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.Multipart;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Strings;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.security.User;
import com.logicaldoc.util.Context;
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

@Path("/")
@Tag(name = "document")
// @Api(value = "document", authorizations = {@Authorization(value = "basic")} )
@Consumes({ MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON })
@Produces({ MediaType.APPLICATION_JSON })
public class RestDocumentService extends SoapDocumentService implements DocumentService {

	private static Logger log = LoggerFactory.getLogger(RestDocumentService.class);
	
    /**
     * Creates a new document using the metadata document object provided as JSON/XML
     * 
     * @param document  the document's metadata
     * @param contentDetail the file content
     * 
     * @throws Exception a generic error
     *
     */
	@Override
    @POST
    @Path("/create")
    @Consumes(MediaType.MULTIPART_FORM_DATA)
    @Produces(MediaType.APPLICATION_JSON)
	@Operation(operationId =  "createDocument", summary = "Creates a new document", description = "Creates a new document using the metadata document object provided as JSON/XML")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = WSDocument.class))),
        @ApiResponse(responseCode = "401", description = "Authentication failed"),
        @ApiResponse(responseCode = "500", description = "Generic error, see the response message") })
	@RequestBody(content = @Content(
			mediaType = MediaType.MULTIPART_FORM_DATA,
			schema = @Schema(implementation = CreateDocumentMultipartRequest.class),
			encoding = @Encoding(name = "file", contentType = "application/octet-stream"))
	)	
    public WSDocument create(
    		@Multipart(value = "document", required = true, type = "application/json") WSDocument document,  
    		@Multipart(value = "content" , required = true, type = "application/octet-stream") Attachment contentDetail) throws Exception {
		
		log.debug("createDocument()");
        
		String sid = validateSession();

		log.debug("document: {}", document);
		log.debug("contentDetail: {}", contentDetail);
		
		DataHandler content = contentDetail.getDataHandler();

		try {
			return super.create(sid, document, content);
		} catch (Exception e) {
			log.error(e.getMessage(), e);
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
	public WSDocument getDocument(@QueryParam("docId") long docId) throws Exception {
		String sid = validateSession();
		return super.getDocument(sid, docId);
	}

	@Override
	@POST
	@Path("/checkout")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Operation(summary = "Checkout a document", description = "Performs the checkout operation on a document. The document status will be changed to checked-out")
	public void checkout(@FormParam("docId") long docId) throws Exception {
		String sid = validateSession();
		super.checkout(sid, docId);
	}

/*	
	@Override
	@POST
	@Path("/checkin")
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@Produces({ MediaType.TEXT_PLAIN, MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
	@Operation(summary = "Check-in an existing document", description = "Performs a check-in (commit) operation of new content over an existing document. The document must be in checked-out status")
//	@ApiImplicitParams({
//			@ApiImplicitParam(name = "docId", value = "The ID of an existing document to update", required = true, dataType = "integer", paramType = "form"),
//			@ApiImplicitParam(name = "comment", value = "An optional comment", required = false, dataType = "string", paramType = "form"),
//			@ApiImplicitParam(name = "release", value = "Indicates whether to create or not a new major release of the updated document", required = false, dataType = "string", paramType = "form", allowableValues = "true, false"),
//			@ApiImplicitParam(name = "filename", value = "File name", required = true, dataType = "string", paramType = "form"),
//			@ApiImplicitParam(name = "filedata", value = "File data", required = true, dataType = "file", paramType = "form") })
	@Parameters({
		@Parameter(name = "docId", description = "The ID of an existing document to update", required = true,
				content = @Content(mediaType = "text/plain"), schema = @Schema(type = "integer")),
		@Parameter(name = "comment", description = "An optional comment", required = false,
			content = @Content(mediaType = "text/plain")),
		@Parameter(name = "release", description = "Indicates whether to create or not a new major release of the updated document", required = false,
			content = @Content(mediaType = "text/plain")),
		@Parameter(name = "filename", description = "File name", required = true,
			content = @Content(mediaType = "text/plain")),
		@Parameter(name = "filedata", description = "File data", required = true,
				content = @Content(mediaType = "application/octet-stream", schema = @Schema(type = "string", format = "binary")))
		})	
    @ApiResponses(value = { 
            @ApiResponse(responseCode = "401", description = "Authentication failed"),
            @ApiResponse(responseCode = "500", description = "Generic error, see the response message") })	
	public Response checkin(@Parameter(hidden = true) List<Attachment> attachments) throws Exception {
		String sid = validateSession();
		try {
			long docId = 0L;
			String comment = null;
			boolean release = false;
			String filename = null;
			DataHandler datah = null;

			for (Attachment att : attachments) {
				Map<String, String> params = att.getContentDisposition().getParameters();
				if ("docId".equals(params.get("name"))) {
					docId = Long.parseLong(att.getObject(String.class));
				} else if ("comment".equals(params.get("name"))) {
					comment = att.getObject(String.class);
				} else if ("release".equals(params.get("name"))) {
					release = Boolean.parseBoolean(att.getObject(String.class));
				} else if ("filename".equals(params.get("name"))) {
					filename = att.getObject(String.class);
				} else if ("filedata".equals(params.get("name"))) {
					datah = att.getDataHandler();
				}
			}

			super.checkin(sid, docId, comment, filename, release, datah);
			return Response.ok("file checked-in").build();
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			return Response.status(500).entity(t.getMessage()).build();
		}
	}
	*/
	
	@Override	
    @POST
    @Path("/checkin")
    @Consumes(MediaType.MULTIPART_FORM_DATA)
    @Operation(summary = "Check-in an existing document", description = "Performs a check-in (commit) operation of new content over an existing document. The document must be in checked-out status")
    @ApiResponses(value = {     		
    	@ApiResponse(responseCode = "204", description = "Successful operation"),	
        @ApiResponse(responseCode = "401", description = "Authentication failed"),
        @ApiResponse(responseCode = "500", description = "Generic error, see the response message") })
	@RequestBody(content = @Content(
			mediaType = MediaType.MULTIPART_FORM_DATA,
			schema = @Schema(implementation = CheckinDocumentMultipartRequest.class),
			encoding = @Encoding(name = "file", contentType = "application/octet-stream"))
	)	    
    public void checkin(
    		@Multipart(value = "docId", required = true) String docId, 
    		@Multipart(value = "comment", required = false) String comment, 
    		@Multipart(value = "release", required = false) String releaseStr, 
    		@Multipart(value = "filename", required = true) String filename,  
    		@Multipart(value = "filedata" , required = true) Attachment filedataDetail) throws Exception {
    	
		String sid = validateSession();
		
		try {
			boolean release = false;
			if (!Strings.isNullOrEmpty(releaseStr)) {
				release = Boolean.parseBoolean(releaseStr);
			}
				
			DataHandler datah = filedataDetail.getDataHandler();

			super.checkin(sid, Long.parseLong(docId), comment, filename, release, datah);
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			throw new WebApplicationException(t.getMessage(), 500);
		}    	
    }
    
	public class CheckinDocumentMultipartRequest {

		@Schema(type = "integer", required = true, description = "The ID of an existing document to update")
		public String docId;		
		
		@Schema(type = "string", required = false, description = "An optional comment")
		public String comment;
		
		@Schema(type = "string", required = false, allowableValues = { "true", "false" }, description = "Indicates whether to create or not a new major release of")
		public String release;
		
		@Schema(type = "string", required = true, description = "File name")
		public String filename;
		
		@Schema(type = "string", format = "binary", required = true, description = "File data")
		public Attachment filedata;
	}    

/*
	@Override
	@POST
	@Path("/upload")
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@Produces({ MediaType.TEXT_PLAIN, MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
	@Operation(summary = "Uploads a document", description = "Creates or updates an existing document, if used in update mode docId must be provided, when used in create mode folderId is required. Returns the ID of the created/updated document. &lt;br/&gt;Example: curl -u admin:admin -H ''Accept: application/json'' -X POST -F folderId=4 -F filename=newDoc.txt -F filedata=@newDoc.txt http://localhost:8080/services/rest/document/upload")
//	@ApiImplicitParams({
//			@ApiImplicitParam(name = "docId", value = "The ID of an existing document to update", required = false, dataType = "integer", paramType = "form"),
//			@ApiImplicitParam(name = "folderId", value = "Folder ID where to place the document", required = false, dataType = "string", paramType = "form"),
//			@ApiImplicitParam(name = "release", value = "Indicates whether to create or not a new major release of an updated document", required = false, dataType = "string", paramType = "form", allowableValues = "true, false"),
//			@ApiImplicitParam(name = "filename", value = "File name", required = true, dataType = "string", paramType = "form"),
//			@ApiImplicitParam(name = "language", value = "Language of the document (ISO 639-2)", required = false, dataType = "string", paramType = "form", defaultValue = "en"),
//			@ApiImplicitParam(name = "filedata", value = "File data", required = true, dataType = "file", paramType = "form") })
//	@ApiResponses(value = { @ApiResponse(code = 401, message = "Authentication failed"),
//			@ApiResponse(code = 500, message = "Generic error, see the response message") })
    @ApiResponses(value = { 
            @ApiResponse(responseCode = "401", description = "Authentication failed"),
            @ApiResponse(responseCode = "500", description = "Generic error, see the response message") })	
	public Response upload(@Parameter(hidden = true) List<Attachment> attachments) throws Exception {
		String sid = validateSession();
		try {
			Long docId = null;
			Long folderId = null;
			boolean release = false;
			String filename = null;
			String language = null;
			DataHandler datah = null;

			for (Attachment att : attachments) {
				Map<String, String> params = att.getContentDisposition().getParameters();
				// log.debug("keys: {}", params.keySet());
				// log.debug("name: {}", params.get("name"));

				if ("docId".equals(params.get("name"))) {
					docId = Long.parseLong(att.getObject(String.class));
				} else if ("folderId".equals(params.get("name"))) {
					folderId = Long.parseLong(att.getObject(String.class));
				} else if ("release".equals(params.get("name"))) {
					release = Boolean.parseBoolean(att.getObject(String.class));
				} else if ("filename".equals(params.get("name"))) {
					filename = att.getObject(String.class);
				} else if ("language".equals(params.get("name"))) {
					language = att.getObject(String.class);
				} else if ("filedata".equals(params.get("name"))) {
					datah = att.getDataHandler();
				}
			}

			long documentId = super.upload(sid, docId, folderId, release, filename, language, datah);
			return Response.ok("" + documentId).build();
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			return Response.status(500).entity(t.getMessage()).build();
		}
	}
*/	
	
	@Override	
    @POST
    @Path("/upload")
    @Consumes(MediaType.MULTIPART_FORM_DATA)
    @Operation(summary = "Uploads a document", description = "Creates or updates an existing document, if used in update mode docId must be provided, when used in create mode folderId is required. Returns the ID of the created/updated document. &lt;br/&gt;Example: curl -u admin:admin -H ''Accept: application/json'' -X POST -F folderId=4 -F filename=newDoc.txt -F filedata=@newDoc.txt http://localhost:8080/services/rest/document/upload")   
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Long.class, description = "The ID of the document created or updated"))),    		
        @ApiResponse(responseCode = "401", description = "Authentication failed"),
        @ApiResponse(responseCode = "500", description = "Generic error, see the response message") })
	@RequestBody(content = @Content(
			mediaType = MediaType.MULTIPART_FORM_DATA,
			schema = @Schema(implementation = UploadDocumentMultipartRequest.class),
			encoding = @Encoding(name = "file", contentType = "application/octet-stream"))
	)	       
    public Long upload(
    		@Multipart(value = "docId", required = false) Integer docId, @Multipart(value = "folderId", required = false) String folderId, 
    		@Multipart(value = "release", required = false)  String release, @Multipart(value = "filename", required = true)  String filename, 
    		@Multipart(value = "language", required = false)  String language,  @Multipart(value = "filedata" , required = true) Attachment filedataDetail) throws Exception {
    	
		String sid = validateSession();
		try {
			Long doc_id = null;
			Long folder_id = null;
			boolean release_bool = false;
			
			if (docId != null) {
				doc_id = Long.parseLong(docId);
			}						
			if (!Strings.isNullOrEmpty(folderId)) {
				folder_id = Long.parseLong(folderId);
			}						
			if (!Strings.isNullOrEmpty(release)) {
				release_bool = Boolean.parseBoolean(release);
			}			
			DataHandler datah = filedataDetail.getDataHandler();

			return super.upload(sid, doc_id, folder_id, release_bool, filename, language, datah);
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			throw new WebApplicationException(t.getMessage(), 500);
		}    	
    }
    
//	@ApiImplicitParams({
//	@ApiImplicitParam(name = "docId", value = "The ID of an existing document to update", required = false, dataType = "integer", paramType = "form"),
//	@ApiImplicitParam(name = "folderId", value = "Folder ID where to place the document", required = false, dataType = "string", paramType = "form"),
//	@ApiImplicitParam(name = "release", value = "Indicates whether to create or not a new major release of an updated document", required = false, dataType = "string", paramType = "form", allowableValues = "true, false"),
//	@ApiImplicitParam(name = "filename", value = "File name", required = true, dataType = "string", paramType = "form"),
//	@ApiImplicitParam(name = "language", value = "Language of the document (ISO 639-2)", required = false, dataType = "string", paramType = "form", defaultValue = "en"),
//	@ApiImplicitParam(name = "filedata", value = "File data", required = true, dataType = "file", paramType = "form") })     
    
	public class UploadDocumentMultipartRequest {

		@Schema(type = "integer", required = false, description = "The ID of an existing document to update")
		public String docId;		
		
		@Schema(type = "string", required = false, description = "Folder ID where to place the document")
		public String folderId;
		
		@Schema(type = "string", required = false, allowableValues = { "true", "false" }, description = "Indicates whether to create or not a new major release of an updated document")
		public String release;
		
		@Schema(type = "string", required = true, description = "File name")
		public String filename;
		
		@Schema(type = "string", required = false, defaultValue = "en", description = "Language of the document (ISO 639-2)")
		public String language;		
		
		@Schema(type = "string", format = "binary", required = true, description = "File data")
		public Attachment filedata;
	}      

/*	
	@Override
	@POST
	@Path("/replaceFile")
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@Produces({ MediaType.TEXT_PLAIN, MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
	@Operation(summary = "Replace the file of a version", description = "Replaces the file associated to a given version.")
//	@ApiImplicitParams({
//			@ApiImplicitParam(name = "docId", value = "The ID of an existing document to update", required = true, dataType = "integer", paramType = "form"),
//			@ApiImplicitParam(name = "fileVersion", value = "The file version", required = false, dataType = "string", paramType = "form"),
//			@ApiImplicitParam(name = "comment", value = "Comment", required = false, dataType = "string", paramType = "form"),
//			@ApiImplicitParam(name = "filedata", value = "File data", required = true, dataType = "file", paramType = "form") })
//	@ApiResponses(value = { @ApiResponse(code = 401, message = "Authentication failed"),
//			@ApiResponse(code = 500, message = "Generic error, see the response message") })
    @ApiResponses(value = { 
            @ApiResponse(responseCode = "401", description = "Authentication failed"),
            @ApiResponse(responseCode = "500", description = "Generic error, see the response message") })	
	public Response replaceFile(@Parameter(hidden = true) List<Attachment> attachments) throws Exception {
		String sid = validateSession();
		try {
			Long docId = null;
			String fileVersion = null;
			String comment = null;
			DataHandler datah = null;

			for (Attachment att : attachments) {
				Map<String, String> params = att.getContentDisposition().getParameters();
				if ("docId".equals(params.get("name"))) {
					docId = Long.parseLong(att.getObject(String.class));
				} else if ("fileVersion".equals(params.get("name"))) {
					fileVersion = att.getObject(String.class);
				} else if ("comment".equals(params.get("name"))) {
					comment = att.getObject(String.class);
				} else if ("filedata".equals(params.get("name"))) {
					datah = att.getDataHandler();
				}
			}

			super.replaceFile(sid, docId, fileVersion, comment, datah);
			return Response.ok("" + docId).build();
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			return Response.status(500).entity(t.getMessage()).build();
		}
	} */
	
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
    @ApiResponses(value = { 
    	@ApiResponse(responseCode = "204", description = "successful operation"),
    	@ApiResponse(responseCode = "400", description = "bad request"),    		
        @ApiResponse(responseCode = "401", description = "Authentication failed"),
        @ApiResponse(responseCode = "500", description = "Generic error, see the response message") })
//	@ApiImplicitParam(name = "docId", value = "The ID of an existing document to update", required = true, dataType = "integer", paramType = "form"),
//	@ApiImplicitParam(name = "fileVersion", value = "The file version", required = false, dataType = "string", paramType = "form"),
//	@ApiImplicitParam(name = "comment", value = "Comment", required = false, dataType = "string", paramType = "form"),
//	@ApiImplicitParam(name = "filedata", value = "File data", required = true, dataType = "file", paramType = "form") })    
	@RequestBody(content = @Content(
			mediaType = MediaType.MULTIPART_FORM_DATA,
			schema = @Schema(implementation = ReplaceFileMultipartRequest.class),
			encoding = @Encoding(name = "file", contentType = "application/octet-stream"))
	)	    
    public void replaceFile(
    		@Multipart(value = "docId", required = true)  Integer docId, 
    		@Multipart(value = "fileVersion", required = false)  String fileVersion, 
    		@Multipart(value = "comment", required = false)  String comment,  
    		@Multipart(value = "filedata" , required = true) Attachment filedataDetail)  throws Exception {
    	
		String sid = validateSession();
		
		try {
			DataHandler datah = filedataDetail.getDataHandler();

			super.replaceFile(sid, docId, fileVersion, comment, datah);
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			throw new WebApplicationException(t.getMessage(), 500);
		}		
    }
    
//	@ApiImplicitParam(name = "docId", value = "The ID of an existing document to update", required = true, dataType = "integer", paramType = "form"),
//	@ApiImplicitParam(name = "fileVersion", value = "The file version", required = false, dataType = "string", paramType = "form"),
//	@ApiImplicitParam(name = "comment", value = "Comment", required = false, dataType = "string", paramType = "form"),
//	@ApiImplicitParam(name = "filedata", value = "File data", required = true, dataType = "file", paramType = "form") })      
	public class ReplaceFileMultipartRequest {

		@Schema(type = "integer", required = true, description = "The ID of an existing document to update")
		public String docId;		
		
		@Schema(type = "string", required = false, description = "The file version")
		public String fileVersion;
		
		@Schema(type = "string", required = false, description = "Comment")
		public String Comment;		
		
		@Schema(type = "string", format = "binary", required = true, description = "File data")
		public Attachment filedata;
	}     


	@Override
	@DELETE
	@Path("/delete")
	@Operation(summary = "Deletes a document")
	public void delete(@Parameter(description = "Document ID to delete", required = true) @QueryParam("docId") long docId)
			throws Exception {
		String sid = validateSession();
		super.delete(sid, docId);
	}

	@Override
	@GET
	@Path("/list")
	@Produces({ MediaType.APPLICATION_JSON })
	@Operation(summary = "Lists documents by folder", description = "Lists Documents by folder identifier")
	public WSDocument[] list(@QueryParam("folderId") long folderId) throws Exception {
		String sid = validateSession();
		return super.listDocuments(sid, folderId, null);
	}

	@Override
	@GET
	@Path("/listDocuments")
	@Operation(summary = "Lists documents by folder and filename", description = "Lists Documents by folder ID filtering the results by filename")
	public WSDocument[] listDocuments(@QueryParam("folderId") long folderId, @QueryParam("fileName") String fileName)
			throws Exception {
		String sid = validateSession();
		return super.listDocuments(sid, folderId, fileName);
	}

	@GET
	@Path("/getContent")
	@Produces(MediaType.APPLICATION_OCTET_STREAM)
	@Operation(summary = "Gets the document content", description = "Returns the content of a document using the document ID in input")	
    @ApiResponses(value = { 
            @ApiResponse(description = "default response", 
            		content = @Content(mediaType = "application/octet-stream", 
            		schema = @Schema(type = "string", format = "binary"))) })	
	public DataHandler getContent(@QueryParam("docId") long docId) throws Exception {
		String sid = validateSession();
		return super.getContent(sid, docId);
	}

	@GET
	@Path("/getVersionContent")
	@Produces(MediaType.APPLICATION_OCTET_STREAM)
	@Operation(summary = "Gets the document content by version", description = "Returns the content of a document using the document ID and version")
    @ApiResponses(value = { 
            @ApiResponse(description = "default response", 
            		content = @Content(mediaType = "application/octet-stream", 
            		schema = @Schema(type = "string", format = "binary"))) })	
	public DataHandler getVersionContent(@QueryParam("docId") long docId, @QueryParam("version") String version)
			throws Exception {
		String sid = validateSession();
		return super.getVersionContent(sid, docId, version);
	}

	@DELETE
	@Path("/deleteVersion")
	@Operation(summary = "Delete the version of a document", description = "Deletes the version of a document using the document ID and version."
			+ " You can not delete the latest version of a document. Returns the latest version of the document")
	public String deleteVersion(
			@QueryParam("docId") @Parameter(description = "Document ID", required = true) long docId,
			@QueryParam("version") String version) throws Exception {
		String sid = validateSession();
		return super.deleteVersion(sid, docId, version);
	}

	@Override
	@PUT
	@Path("/update")
	@Operation(summary = "Updates an existing document", description = "Updates the metadata of an existing document. The ID of the document must be specified in the WSDocument value object. The provided example moves document with ID 1111111 to folder 3435433")
	public void update(
			@Parameter(description = "Document object that needs to be updated", required = true, 
			example = "{ \"id\": 1111111, \"folderId\": 3435433 }" ) WSDocument document)
			throws Exception {
		String sid = validateSession();
		super.update(sid, document);
	}

	@Override
	@POST
	@Path("/addNote")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Operation(summary = "Adds a new note for the given document")
	public WSNote addNote(
			@FormParam("docId") @Parameter(description = "Document ID", required = true) long docId,
			@FormParam("note") @Parameter(description = "Text of the note to add", required = true) String note)
			throws Exception {
		String sid = validateSession();
		return super.addNote(sid, docId, note);
	}

	/**
	 * Adds a new note for the given document
	 */
	@DELETE
	@Path("/deleteNote")
	@Operation(summary = "Deletes a note")
	public void deleteNote(
			@QueryParam("noteId") @Parameter(description = "ID of the note to delete", required = true) long noteId)
			throws Exception {
		String sid = validateSession();
		super.deleteNote(sid, noteId);
	}

	/**
	 * Gets the notes for the given document
	 */
	@GET
	@Path("/getNotes")
	@Operation(summary = "Gets all the notes of a document")
	public WSNote[] getNotes(@QueryParam("docId") @Parameter(description = "Document ID", required = true) long docId)
			throws Exception {
		String sid = validateSession();
		return super.getNotes(sid, docId);
	}

	/**
	 * Puts a new rating on the given document
	 */
	@PUT
	@Path("/rateDocument")
	@Operation(summary = "Add/Update the user's vote for a document")
	public WSRating rateDocument(
			@QueryParam("docId") @Parameter(description = "Document ID", required = true) long docId,
			@QueryParam("vote") @Parameter(description = "The user's vote", required = true) int vote) throws Exception {
		String sid = validateSession();
		return super.rateDocument(sid, docId, vote);
	}

	/**
	 * Gets all the ratings of the given document
	 */
	@GET
	@Path("/getRatings")
	@Operation(summary = "Retrieves the different ratings of a focuments")
	public WSRating[] getRatings(@QueryParam("docId") @Parameter(description = "Document ID", required = true) long docId)
			throws Exception {
		String sid = validateSession();
		return super.getRatings(sid, docId);
	}

	@Override
	@PUT
	@Path("/move")
	@Operation(summary = "Moves an existing document with the given identifier")
	public void move(
			@QueryParam("docId") @Parameter(description = "Document ID", required = true) long docId,
			@QueryParam("folderId") @Parameter(description = "Target Folder ID", required = true) long folderId)
			throws Exception {
		String sid = validateSession();
		super.move(sid, docId, folderId);
	}

	@Override
	@PUT
	@Path("/move")
	@Operation(summary = "Copies a document into a folder")
	@Produces(MediaType.APPLICATION_JSON)
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = WSDocument.class))),
        @ApiResponse(responseCode = "401", description = "Authentication failed"),
        @ApiResponse(responseCode = "500", description = "Generic error, see the response message") })
	public WSDocument copy(
			@QueryParam("docId") @Parameter(description = "Document ID", required = true) long docId,
			@QueryParam("folderId") @Parameter(description = "Target Folder ID", required = true) long folderId)
			throws Exception {
		String sid = validateSession();
		return super.copy(sid, docId, folderId);
	}
	
	
	
	@Override
	@PUT
	@Path("/createThumbnail")
	@Operation(summary = "Creates the thumbail of the given document; if the thumbnail was already created, nothing will happen")
	public void createThumbnail(
			@QueryParam("docId") long docId, @QueryParam("fileVersion") String fileVersion,
			@QueryParam("type") String type) throws Exception {
		String sid = validateSession();
		super.createThumbnail(sid, docId, fileVersion, type);
	}

	@Override
	@PUT
	@Path("/createPdf")
	@Operation(summary = "Creates the PDF conversion of the given document; if the PDF conversion was already created, nothing will happen")
	public void createPdf(@QueryParam("docId") long docId, @QueryParam("fileVersion") String fileVersion)
			throws Exception {
		String sid = validateSession();
		super.createPdf(sid, docId, fileVersion);
	}

	@Override
	@PUT
	@Path("/promoteVersion")
	@Operation(summary = "Promotes an old version to the current default one")
	public void promoteVersion(
			@QueryParam("docId") @Parameter(description = "Document ID", required = true) long docId,
			@QueryParam("version") String version) throws Exception {
		String sid = validateSession();
		super.promoteVersion(sid, docId, version);
	}

	@Override
	@PUT
	@Path("/rename")
	@Operation(summary = "Renames the title of an existing document with the given identifier")
	public void rename(
			@QueryParam("docId") @Parameter(description = "Document ID", required = true) long docId,
			@QueryParam("name") @Parameter(description = "The new document filename", required = true) String name)
			throws Exception {
		String sid = validateSession();
		super.rename(sid, docId, name);
	}

	@Override
	@GET
	@Path("/getVersions")
	@Operation(summary = "Gets the versions", description = "Gets the version history of an existing document with the given identifier")
	public WSDocument[] getVersions(@QueryParam("docId") @Parameter(description = "Document ID", required = true) long docId)
			throws Exception {
		String sid = validateSession();
		return super.getVersions(sid, docId);
	}

	@Override
	@POST
	@Path("/createAlias")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Operation(summary = "Creates a new document alias", description = "Creates a new document alias for the given document inside a specified folder")
	public WSDocument createAlias(
			@FormParam("docId") @Parameter(description = "Source document ID", required = true) long docId,
			@FormParam("folderId") @Parameter(description = "Target folder ID", required = true) long folderId,
			@FormParam("type") @Parameter(description = "Type of the alias (use 'pdf' to create an alias to the PDF conversion)", required = true) String type)
			throws Exception {
		String sid = validateSession();
		return super.createAlias(sid, docId, folderId, type);
	}

	@Override
	@POST
	@Path("/createDownloadTicket")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Operation(summary = "Creates a new download ticket", description = "Creates a new download ticket to the original document or it's PDF conversion")
	public String createDownloadTicket(
			@FormParam("docId") @Parameter(description = "Source document ID", required = true) long docId,
			@FormParam("suffix") @Parameter(description = "can be null or 'conversion.pdf'") String suffix,
			@FormParam("expireHours") @Parameter(description = "expiration time expressed in hours") Integer expireHours,
			@FormParam("expireDate") @Parameter(description = "exact expiration date expressed in the format yyyy-MM-dd") String expireDate,
			@FormParam("maxDownloads") @Parameter(description = "maximum number of downloads allowed", required = true) Integer maxDownloads)
			throws Exception {
		String sid = validateSession();
		return super.createDownloadTicket(sid, docId, suffix, expireHours, expireDate, maxDownloads);
	}

	@Override
	@DELETE
	@Path("/deleteLink")
	@Operation(summary = "Removes an existing link")
	public void deleteLink(@Parameter(description = "ID of the link", required = true) @QueryParam("id") long id)
			throws Exception {
		String sid = validateSession();
		super.deleteLink(sid, id);
	}

	@Override
	@GET
	@Path("/getAliases")
	@Operation(summary = "Gets the aliases", description = "Gets the aliases of the given document; returns an array of WSDocument that are aliases")
	public WSDocument[] getAliases(
			@Parameter(description = "The document ID", required = true) @QueryParam("docId") long docId) throws Exception {
		String sid = validateSession();
		return super.getAliases(sid, docId);
	}

	@Override
	@GET
	@Path("/getDocumentByCustomId")
	@Operation(summary = "Gets document metadata by custom ID", description = "Gets document metadata of an existing document with the given custom identifier")
	public WSDocument getDocumentByCustomId(
			@Parameter(description = "The custom ID", required = true) @QueryParam("customId") String customId)
			throws Exception {
		String sid = validateSession();
		return super.getDocumentByCustomId(sid, customId);
	}

	@Override
	@GET
	@Path("/getDocuments")
	@Operation(summary = "Gets the metadata of a collection of document", description = "Gets document metadata of a collection of existing documents with the given identifiers; returns an array of WSDocument")
	public WSDocument[] getDocuments(
			@QueryParam("docIds") @Parameter(description = "Array of document IDs", required = true) Long[] docIds)
			throws Exception {
		String sid = validateSession();
		return super.getDocuments(sid, docIds);
	}

	@Override
	@GET
	@Path("/getExtractedText")
	@Produces({ MediaType.TEXT_PLAIN })
	@Operation(summary = "Gets the extracted text of a document", description = "Gets the document's text stored in the full-text index")
	public String getExtractedText(
			@QueryParam("docId") @Parameter(description = "The document ID", required = true) long docId) throws Exception {
		String sid = validateSession();
		return super.getExtractedText(sid, docId);
	}

	@Override
	@GET
	@Path("/getRecentDocuments")
	@Operation(summary = "Gets the last modified documents", description = "Lists of last modified documents of the current session")
	public WSDocument[] getRecentDocuments(
			@QueryParam("maxHits") @Parameter(description = "Maximum number of returned records", required = true) Integer maxHits)
			throws Exception {
		String sid = validateSession();
		return super.getRecentDocuments(sid, maxHits);
	}

	@Override
	@GET
	@Path("/getLinks")
	@Operation(summary = "Gets the links of a document", description = "Gets all the links of a specific document; returns an array of links")
	public WSLink[] getLinks(@QueryParam("docId") @Parameter(description = "The document ID", required = true) long docId)
			throws Exception {
		String sid = validateSession();
		return super.getLinks(sid, docId);
	}

	@Override
	@GET
	@Path("/getResource")
	@Produces(MediaType.APPLICATION_OCTET_STREAM)
	@Operation(summary = "Gets the content of a resource", description = "Gets the content of a resource associated to the given document; returns the raw content of the file")
    @ApiResponses(value = { 
            @ApiResponse(description = "default response",
            		content = @Content(mediaType = "application/octet-stream", 
            		schema = @Schema(type = "string", format = "binary"))) })	
	public DataHandler getResource(
			@QueryParam("docId") @Parameter(description = "The document ID", required = true) long docId,
			@QueryParam("fileVersion") @Parameter(description = "The file version to retrieve") String fileVersion,
			@QueryParam("suffix") @Parameter(description = "suffix specification(it cannot be empty, use 'conversion.pdf' to get the PDF conversion)") String suffix)
			throws Exception {
		String sid = validateSession();
		return super.getResource(sid, docId, fileVersion, suffix);
	}

	@Override
	@GET
	@Path("/isReadable")
	@Operation(summary = "Tests if a document is readable", description = "Tests if a document is readable; returns True if the identifier denotes a document, otherwise false")
	public boolean isReadable(@QueryParam("docId") @Parameter(description = "Document ID", required = true) long docId)
			throws Exception {
		String sid = validateSession();
		return super.isReadable(sid, docId);
	}

	@Override
	@POST
	@Path("/link")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Operation(summary = "Creates a link between two documents", description = "Creates a new link between two documents; returns the created link object")
	public WSLink link(
			@FormParam("doc1") @Parameter(description = "ID of document 1", required = true) long doc1,
			@FormParam("doc2") @Parameter(description = "ID of document 2", required = true) long doc2,
			@FormParam("type") @Parameter(description = "type of the link (use 'pdf' to point to the pdf conversion)") String type)
			throws Exception {
		String sid = validateSession();
		return super.link(sid, doc1, doc2, type);
	}

	@Override
	@PUT
	@Path("/lock")
	@Operation(summary = "Locks a document", description = "Locks an existing document with the given identifier")
	public void lock(@QueryParam("docId") @Parameter(description = "Document ID", required = true) long docId)
			throws Exception {
		String sid = validateSession();
		super.lock(sid, docId);
	}

	@Override
	@POST
	@Path("/reindex")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Operation(summary = "Re-indexes a document", description = "re-indexes(or indexes from scratch) a document")
	public void reindex(
			@FormParam("doc1") @Parameter(description = "Document ID", required = true) long docId,
			@FormParam("content") @Parameter(description = "Document ID") String content) throws Exception {
		String sid = validateSession();
		super.reindex(sid, docId, content);
	}

	@Override	
    @POST
    @Path("/uploadResource")
    @Consumes(MediaType.MULTIPART_FORM_DATA)
	@Operation(summary = "Uploads a new resource of the document", description = "Uploads a new resource attached to the given document. If the resource already exists it is overwritten")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "204", description = "successful operation") })
	@RequestBody(content = @Content(
			mediaType = MediaType.MULTIPART_FORM_DATA,
			schema = @Schema(implementation = UploadResourceMultipartRequest.class),
			encoding = @Encoding(name = "file", contentType = "application/octet-stream"))
	)	    	
    public void uploadResource(
    		@Multipart(value = "docId", required = true)  Integer docId, 
    		@Multipart(value = "fileVersion", required = false)  String fileVersion, 
    		@Multipart(value = "suffix", required = true)  String suffix,  
    		@Multipart(value = "content" , required = true) Attachment contentDetail)  throws Exception {
		
		String sid = validateSession();

		DataHandler datah = contentDetail.getDataHandler();

		super.uploadResource(sid, docId, fileVersion, suffix, datah);		
	}
	
//	@ApiImplicitParam(name = "docId", value = "the document id", required = true, dataType = "integer", paramType = "form"),
//	@ApiImplicitParam(name = "fileVersion", value = "the specific file version", required = false, dataType = "string", paramType = "form"),
//	@ApiImplicitParam(name = "suffix", value = "suffix specification(it cannot be empty, use 'conversion.pdf' to put the PDF conversion)", required = true, dataType = "string", paramType = "form"),
//	@ApiImplicitParam(name = "content", value = "raw content of the file", required = true, dataType = "file", paramType = "form"), })      
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
	public void restore(
			@QueryParam("docId") @Parameter(description = "Document ID", required = true) long docId,
			@QueryParam("folderId") @Parameter(description = "Folder ID (target)", required = true) long folderId)
			throws Exception {
		String sid = validateSession();
		super.restore(sid, docId, folderId);
	}

	@Override
	@POST
	@Path("/saveNote")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Operation(summary = "Adds a new note", description = "Adds/modifies a note for the given document")
	public WSNote saveNote(
			@FormParam("docId") @Parameter(description = "Document ID", required = true) long docId,
			@FormParam("note") @Parameter(description = "the WSNote representation as json string", required = true) WSNote note)
			throws Exception {
		String sid = validateSession();
		return super.saveNote(sid, docId, note);
	}

	@Override
	@POST
	@Path("/sendEmail")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Operation(summary = "Sends documents by email", description = "Sends a set of documents as mail attachments")
	public void sendEmail(
			@FormParam("docIds") @Parameter(description = "Document IDs", required = true) Long[] docIds,
			@FormParam("recipients") @Parameter(description = "Set of recipients(comma separated)", required = true) String recipients,
			@FormParam("subject") @Parameter(description = "The email subject") String subject,
			@FormParam("message") @Parameter(description = "The email message body") String message) throws Exception {
		String sid = validateSession();
		super.sendEmail(sid, docIds, recipients, subject, message);
	}

	@Override
	@POST
	@Path("/setPassword")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Operation(summary = "Password protect a document", description = "Protects with a password the given document")
	public void setPassword(
			@FormParam("docId") @Parameter(description = "Document ID", required = true) long docId,
			@FormParam("password") @Parameter(description = "A password", required = true) String password) throws Exception {
		String sid = validateSession();
		super.setPassword(sid, docId, password);
	}

	@Override
	@PUT
	@Path("/unlock")
	@Operation(summary = "Unlocks the document", description = "Unlocks an existing document with the given identifier")
	public void unlock(@QueryParam("docId") @Parameter(description = "Document ID", required = true) long docId)
			throws Exception {
		String sid = validateSession();
		super.unlock(sid, docId);
	}

	@Override
	@POST
	@Path("/unsetPassword")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Operation(summary = "Removes password protection", description = "Removes the password protection from the document")
	public void unsetPassword(
			@FormParam("docId") @Parameter(description = "Document ID", required = true) long docId,
			@FormParam("currentPassword") @Parameter(description = "A password", required = true) String currentPassword)
			throws Exception {
		String sid = validateSession();
		super.unsetPassword(sid, docId, currentPassword);
	}

	@Override
	@POST
	@Path("/unprotect")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Operation(summary = "Temporarily removes password protection", description = "Unprotect a document that is password protected. If the given password is right, the document remains unprotected for the duration of the session")
	public boolean unprotect(
			@FormParam("docId") @Parameter(description = "Document ID", required = true) long docId,
			@FormParam("password") @Parameter(description = "A password", required = true) String password) throws Exception {
		String sid = validateSession();
		return super.unprotect(sid, docId, password);
	}

	// @Produces(MediaType.APPLICATION_OCTET_STREAM)
	// "application/octet-stream";
	// @Produces("image/jpeg")

	@Override
	@GET
	@Path("/thumbnail/{type}/{docpath:.*}")
	@Produces("image/png")
    @ApiResponses(value = { 
            @ApiResponse(description = "default response", 
            		content = @Content(mediaType = "image/png", 
            		schema = @Schema(type = "string", format = "binary"))) })		
	public DataHandler getThumbnail(
			@PathParam("type") String type, 
			@PathParam("docpath") String docPath,
			@PathParam("docpath") List<PathSegment> docPathList) throws Exception {
		String sid = validateSession();

		String myPath = "/" + docPath;

		User user = validateSession(sid);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document doc = docDao.findByPath(myPath, user.getTenantId());

		if (!type.toLowerCase().endsWith(".png"))
			type = type += ".png";

		try {
			return super.getResource(sid, doc.getId(), doc.getFileVersion(), type);
		} catch (Exception e) {
			super.createThumbnail(sid, doc.getId(), doc.getFileVersion(), type);
			return super.getResource(sid, doc.getId(), doc.getFileVersion(), type);
		}
	}
}