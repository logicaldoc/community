package com.logicaldoc.webservice.rest.endpoint;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import io.swagger.annotations.Example;
import io.swagger.annotations.ExampleProperty;

import java.util.List;
import java.util.Map;

import javax.activation.DataHandler;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSNote;
import com.logicaldoc.webservice.model.WSRating;
import com.logicaldoc.webservice.rest.DocumentService;
import com.logicaldoc.webservice.soap.endpoint.SoapDocumentService;

@Path("/")
// @Api(value = "document", authorizations = {@Authorization(value = "basic")} )
@Api(value = "document")
@Consumes({ MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON })
@Produces({ MediaType.APPLICATION_JSON })
public class RestDocumentService extends SoapDocumentService implements DocumentService {

	private static Logger log = LoggerFactory.getLogger(RestDocumentService.class);

	@Override
	@POST
	@Path("/create")
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@ApiOperation(nickname = "createDocument", value = "Creates a new document", notes = "Creates a new document using the metadata document object provided as JSON/XML", response = WSDocument.class)
	@ApiImplicitParams({
			@ApiImplicitParam(name = "document", value = "The document metadata provided as WSDocument object encoded in JSON/XML format", required = true, dataType = "string", paramType = "form", examples = @Example(value = { @ExampleProperty(value = "{ \"fileName\":\"Help.pdf\",\"folderId\": 4, \"language\":\"en\" }") })),
			@ApiImplicitParam(name = "content", value = "File data", required = true, dataType = "file", paramType = "form") })
			@ApiResponses(value = { @ApiResponse(code = 401, message = "Authentication failed"),@ApiResponse(code = 500, message = "Generic error, see the response message") })
	public Response create(@ApiParam(hidden = true) List<Attachment> atts) throws Exception {
		log.debug("create()");

		String sid = validateSession();

		WSDocument document = null;
		DataHandler content = null;

		for (Attachment att : atts) {
			if ("document".equals(att.getContentDisposition().getParameter("name"))) {
				document = att.getObject(WSDocument.class);
			} else if ("content".equals(att.getContentDisposition().getParameter("name"))) {
				content = att.getDataHandler();
			}
		}

		log.debug("document: {}", document);
		log.debug("content: {}", content);

		try {
			// return super.create(sid, document, content);
			WSDocument cdoc = super.create(sid, document, content);
			return Response.ok().entity(cdoc).build();
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			return Response.status(500).entity(e.getMessage()).build();
		}
	}

	@Override
	@GET
	@Path("/getDocument")
	@Produces({ MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
	@ApiOperation(value = "Gets document metadata", notes = "Gets the document metadata")
	public WSDocument getDocument(@QueryParam("docId") long docId) throws Exception {
		String sid = validateSession();
		return super.getDocument(sid, docId);
	}

	@Override
	@POST
	@Path("/checkout")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@ApiOperation(value = "Checkout a document", notes = "Performs the checkout operation on a document. The document status will be changed to checked-out")
	public void checkout(@FormParam("docId") long docId) throws Exception {
		String sid = validateSession();
		super.checkout(sid, docId);
	}

	@Override
	@POST
	@Path("/checkin")
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@Produces({ MediaType.TEXT_PLAIN, MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
	@ApiOperation(value = "Check-in an existing document", notes = "Performs a check-in (commit) operation of new content over an existing document. The document must be in checked-out status")
	@ApiImplicitParams({
			@ApiImplicitParam(name = "docId", value = "The ID of an existing document to update", required = true, dataType = "integer", paramType = "form"),
			@ApiImplicitParam(name = "comment", value = "An optional comment", required = false, dataType = "string", paramType = "form"),
			@ApiImplicitParam(name = "release", value = "Indicates whether to create or not a new major release of the updated document", required = false, dataType = "string", paramType = "form", allowableValues = "true, false"),
			@ApiImplicitParam(name = "filename", value = "File name", required = true, dataType = "string", paramType = "form"),
			@ApiImplicitParam(name = "filedata", value = "File data", required = true, dataType = "file", paramType = "form") })
	@ApiResponses(value = { @ApiResponse(code = 401, message = "Authentication failed"),
			@ApiResponse(code = 500, message = "Generic error, see the response message") })
	public Response checkin(@ApiParam(hidden = true) List<Attachment> attachments) throws Exception {
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

	@Override
	@POST
	@Path("/upload")
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@Produces({ MediaType.TEXT_PLAIN, MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
	@ApiOperation(value = "Uploads a document", notes = "Creates or updates an existing document, if used in update mode docId must be provided, when used in create mode folderId is required. Returns the ID of the created/updated document. &lt;br/&gt;Example: curl -u admin:admin -H ''Accept: application/json'' -X POST -F folderId=4 -F filename=newDoc.txt -F filedata=@newDoc.txt http://localhost:8080/services/rest/document/upload")
	@ApiImplicitParams({
			@ApiImplicitParam(name = "docId", value = "The ID of an existing document to update", required = false, dataType = "integer", paramType = "form"),
			@ApiImplicitParam(name = "folderId", value = "Folder ID where to place the document", required = false, dataType = "string", paramType = "form"),
			@ApiImplicitParam(name = "release", value = "Indicates whether to create or not a new major release of an updated document", required = false, dataType = "string", paramType = "form", allowableValues = "true, false"),
			@ApiImplicitParam(name = "filename", value = "File name", required = true, dataType = "string", paramType = "form"),
			@ApiImplicitParam(name = "language", value = "Language of the document (ISO 639-2)", required = false, dataType = "string", paramType = "form", defaultValue = "en"),
			@ApiImplicitParam(name = "filedata", value = "File data", required = true, dataType = "file", paramType = "form") })
	@ApiResponses(value = { @ApiResponse(code = 401, message = "Authentication failed"),
			@ApiResponse(code = 500, message = "Generic error, see the response message") })
	public Response upload(@ApiParam(hidden = true) List<Attachment> attachments) throws Exception {
		
		log.error("upload");
		
		String sid = validateSession();
		
		log.error("Session validated: " + sid);
		
		try {
			Long docId = null;
			Long folderId = null;
			boolean release = false;
			String filename = null;
			String language = null;
			DataHandler datah = null;

			for (Attachment att : attachments) {
				Map<String, String> params = att.getContentDisposition().getParameters();
				log.error("keys: {}", params.keySet());
				log.error("name: {}", params.get("name"));

				if ("docId".equals(params.get("name"))) {
					docId = Long.parseLong(att.getObject(String.class));
					log.error("docId: {}", docId);
				} else if ("folderId".equals(params.get("name"))) {
					folderId = Long.parseLong(att.getObject(String.class));
					log.error("folderId: {}", folderId);
				} else if ("release".equals(params.get("name"))) {
					release = Boolean.parseBoolean(att.getObject(String.class));
					log.error("release: {}", release);
				} else if ("filename".equals(params.get("name"))) {
					filename = att.getObject(String.class);
					log.error("filename: {}", filename);
				} else if ("language".equals(params.get("name"))) {
					language = att.getObject(String.class);
					log.error("language: {}", language);
				} else if ("filedata".equals(params.get("name"))) {
					log.error("filedata is present");
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

	@Override
	@DELETE
	@Path("/delete")
	@ApiOperation(value = "Deletes a document")
	public void delete(@ApiParam(value = "Document ID to delete", required = true) @QueryParam("docId") long docId)
			throws Exception {
		String sid = validateSession();
		super.delete(sid, docId);
	}

	@Override
	@GET
	@Path("/list")
	@Produces({ MediaType.APPLICATION_JSON })
	@ApiOperation(value = "Lists documents by folder", notes = "Lists Documents by folder identifier", response = WSDocument.class, responseContainer = "List")
	public WSDocument[] list(@QueryParam("folderId") long folderId) throws Exception {
		String sid = validateSession();
		return super.listDocuments(sid, folderId, null);
	}

	@Override
	@GET
	@Path("/listDocuments")
	@ApiOperation(value = "Lists documents by folder and filename", notes = "Lists Documents by folder ID filtering the results by filename", response = WSDocument.class, responseContainer = "List")
	public WSDocument[] listDocuments(@QueryParam("folderId") long folderId, @QueryParam("fileName") String fileName)
			throws Exception {
		String sid = validateSession();
		return super.listDocuments(sid, folderId, fileName);
	}

	@GET
	@Path("/getContent")
	@Produces(MediaType.APPLICATION_OCTET_STREAM)
	@ApiOperation(value = "Gets the document content", notes = "Returns the content of a document using the document ID in input")
	public DataHandler getContent(@QueryParam("docId") long docId) throws Exception {
		String sid = validateSession();
		return super.getContent(sid, docId);
	}

	@GET
	@Path("/getVersionContent")
	@Produces(MediaType.APPLICATION_OCTET_STREAM)
	@ApiOperation(value = "Gets the document content by version", notes = "Returns the content of a document using the document ID and version")
	public DataHandler getVersionContent(@QueryParam("docId") long docId, @QueryParam("version") String version)
			throws Exception {
		String sid = validateSession();
		return super.getVersionContent(sid, docId, version);
	}
	
	@DELETE
	@Path("/deleteVersion")
	@ApiOperation(value = "Delete the version of a document", notes = "Deletes the version of a document using the document ID and version."
			+ " You can not delete the latest version of a document. Returns the latest version of the document")
	public String deleteVersion(@QueryParam("docId") long docId, @QueryParam("version") String version)
			throws Exception {
		String sid = validateSession();
		return super.deleteVersion(sid, docId, version);
	}	

	@Override
	@PUT
	@Path("/update")
	@ApiOperation(value = "Updates an existing document", notes = "Updates the metadata of an existing document. The ID of the document must be specified in the WSDocument value object. The provided example moves document with ID 1111111 to folder 3435433")
	public void update(
			@ApiParam(value = "Document object that needs to be updated", required = true, examples = @Example(value = { @ExampleProperty(value = "{ \"id\": 1111111, \"folderId\": 3435433 }") })) WSDocument document)
			throws Exception {
		String sid = validateSession();
		super.update(sid, document);
	}

	@Override
	@POST
	@Path("/addNote")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED})
	@ApiOperation(value = "Adds a new note for the given document")
	public WSNote addNote(@FormParam("docId") @ApiParam(value = "Document ID", required = true) long docId,
			@FormParam("note") @ApiParam(value = "Text of the note to add", required = true) String note)
			throws Exception {
		String sid = validateSession();
		return super.addNote(sid, docId, note);
	}

	/**
	 * Adds a new note for the given document
	 */
	@DELETE
	@Path("/deleteNote")
	@ApiOperation(value = "Deletes a note")
	public void deleteNote(
			@QueryParam("noteId") @ApiParam(value = "ID of the note to delete", required = true) long noteId)
			throws Exception {
		String sid = validateSession();
		super.deleteNote(sid, noteId);
	}

	/**
	 * Gets the notes for the given document
	 */
	@GET
	@Path("/getNotes")
	@ApiOperation(value = "Gets all the notes of a document")
	public WSNote[] getNotes(@QueryParam("docId") @ApiParam(value = "Document ID", required = true) long docId)
			throws Exception {
		String sid = validateSession();
		return super.getNotes(sid, docId);
	}

	/**
	 * Puts a new rating on the given document
	 */
	@GET
	@Path("/rateDocument")
	@ApiOperation(value = "Add/Update the user's vote for a document")
	public WSRating rateDocument(@QueryParam("docId") @ApiParam(value = "Document ID", required = true) long docId,
			@QueryParam("vote") @ApiParam(value = "The user's vote", required = true) int vote) throws Exception {
		String sid = validateSession();
		return super.rateDocument(sid, docId, vote);
	}

	/**
	 * Gets all the ratings of the given document
	 */
	@GET
	@Path("/getRatings")
	@ApiOperation(value = "Retrieves the different ratings of a focuments")
	public WSRating[] getRatings(@QueryParam("docId") @ApiParam(value = "Document ID", required = true) long docId)
			throws Exception {
		String sid = validateSession();
		return super.getRatings(sid, docId);
	}
	
	@Override
	@PUT
	@Path("/move")
	@ApiOperation(value = "Moves an existing document with the given identifier")
	public void move(@QueryParam("docId") @ApiParam(value = "Document ID", required = true) long docId, @QueryParam("folderId") @ApiParam(value = "Target Folder ID", required = true) long folderId)
			throws Exception {
		String sid = validateSession();
		super.move(sid, docId, folderId);
	}
}