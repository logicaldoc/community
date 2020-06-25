package com.logicaldoc.webservice.rest.endpoint;

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
import com.logicaldoc.webservice.model.WSLink;
import com.logicaldoc.webservice.model.WSNote;
import com.logicaldoc.webservice.model.WSRating;
import com.logicaldoc.webservice.rest.DocumentService;
import com.logicaldoc.webservice.soap.endpoint.SoapDocumentService;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import io.swagger.annotations.Example;
import io.swagger.annotations.ExampleProperty;

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
			@ApiImplicitParam(name = "document", value = "The document metadata provided as WSDocument object encoded in JSON/XML format", required = true, dataType = "string", paramType = "form", examples = @Example(value = {
					@ExampleProperty(value = "{ \"fileName\":\"Help.pdf\",\"folderId\": 4, \"language\":\"en\" }") })),
			@ApiImplicitParam(name = "content", value = "File data", required = true, dataType = "file", paramType = "form") })
	@ApiResponses(value = { @ApiResponse(code = 401, message = "Authentication failed"),
			@ApiResponse(code = 500, message = "Generic error, see the response message") })
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

	@Override
	@POST
	@Path("/replaceFile")
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@Produces({ MediaType.TEXT_PLAIN, MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
	@ApiOperation(value = "Replace the file of a version", notes = "Replaces the file associated to a given version.")
	@ApiImplicitParams({
			@ApiImplicitParam(name = "docId", value = "The ID of an existing document to update", required = true, dataType = "integer", paramType = "form"),
			@ApiImplicitParam(name = "fileVersion", value = "The file version", required = false, dataType = "string", paramType = "form"),
			@ApiImplicitParam(name = "comment", value = "Comment", required = false, dataType = "string", paramType = "form"),
			@ApiImplicitParam(name = "filedata", value = "File data", required = true, dataType = "file", paramType = "form") })
	@ApiResponses(value = { @ApiResponse(code = 401, message = "Authentication failed"),
			@ApiResponse(code = 500, message = "Generic error, see the response message") })
	public Response replaceFile(List<Attachment> attachments) throws Exception {
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
	public String deleteVersion(@QueryParam("docId") @ApiParam(value = "Document ID", required = true) long docId, @QueryParam("version") String version)
			throws Exception {
		String sid = validateSession();
		return super.deleteVersion(sid, docId, version);
	}

	@Override
	@PUT
	@Path("/update")
	@ApiOperation(value = "Updates an existing document", notes = "Updates the metadata of an existing document. The ID of the document must be specified in the WSDocument value object. The provided example moves document with ID 1111111 to folder 3435433")
	public void update(
			@ApiParam(value = "Document object that needs to be updated", required = true, examples = @Example(value = {
					@ExampleProperty(value = "{ \"id\": 1111111, \"folderId\": 3435433 }") })) WSDocument document)
			throws Exception {
		String sid = validateSession();
		super.update(sid, document);
	}

	@Override
	@POST
	@Path("/addNote")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@ApiOperation(value = "Adds a new note for the given document")
	public WSNote addNote(
			@FormParam("docId") @ApiParam(value = "Document ID", required = true) long docId,
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
	@PUT
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
	public void move(@QueryParam("docId") @ApiParam(value = "Document ID", required = true) long docId,
			@QueryParam("folderId") @ApiParam(value = "Target Folder ID", required = true) long folderId)
			throws Exception {
		String sid = validateSession();
		super.move(sid, docId, folderId);
	}

	@Override
	@PUT
	@Path("/createThumbnail")
	@ApiOperation(value = "Creates the thumbail of the given document; if the thumbnail was already created, nothing will happen")
	public void createThumbnail(@QueryParam("docId") long docId, @QueryParam("fileVersion") String fileVersion)
			throws Exception {
		String sid = validateSession();
		super.createThumbnail(sid, docId, fileVersion);
	}

	@Override
	@PUT
	@Path("/createPdf")
	@ApiOperation(value = "Creates the PDF conversion of the given document; if the PDF conversion was already created, nothing will happen")
	public void createPdf(@QueryParam("docId") long docId, @QueryParam("fileVersion") String fileVersion)
			throws Exception {
		String sid = validateSession();
		super.createPdf(sid, docId, fileVersion);
	}

	@Override
	@PUT
	@Path("/promoteVersion")
	@ApiOperation(value = "Promotes an old version to the current default one")
	public void promoteVersion(@QueryParam("docId") @ApiParam(value = "Document ID", required = true) long docId,
			@QueryParam("version") String version) throws Exception {
		String sid = validateSession();
		super.promoteVersion(sid, docId, version);
	}

	@Override
	@PUT
	@Path("/rename")
	@ApiOperation(value = "Renames the title of an existing document with the given identifier")
	public void rename(@QueryParam("docId") @ApiParam(value = "Document ID", required = true) long docId, 
			@QueryParam("name") @ApiParam(value = "The new document filename", required = true) String name) throws Exception {
		String sid = validateSession();
		super.rename(sid, docId, name);
	}

	@Override
	@GET
	@Path("/getVersions")
	@ApiOperation(value="Gets the versions", notes = "Gets the version history of an existing document with the given identifier")
	public WSDocument[] getVersions(@QueryParam("docId") @ApiParam(value = "Document ID", required = true) long docId) throws Exception {
		String sid = validateSession();
		return super.getVersions(sid, docId);
	}

	@Override
	@POST
	@Path("/createAlias")
	@ApiOperation(value="Creates a new document alias", notes = "Creates a new document alias for the given document inside a specified folder")	
	public WSDocument createAlias(
			@FormParam("docId") @ApiParam(value = "Source document ID", required = true) long docId, 
			@FormParam("folderId") @ApiParam(value = "Target folder ID", required = true) long folderId, 
			@FormParam("type") @ApiParam(value = "Type of the alias (use 'pdf' to create an alias to the PDF conversion)", required = true) String type) 
					throws Exception {
		String sid = validateSession();
		return super.createAlias(sid, docId, folderId, type);
	}

	@Override
	@POST
	@Path("/createDownloadTicket")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@ApiOperation(value="Creates a new download ticket", notes = "Creates a new download ticket to the original document or it's PDF conversion")	
	public String createDownloadTicket(
			@FormParam("docId") @ApiParam(value = "Source document ID", required = true) long docId, 
			@FormParam("suffix") @ApiParam(value = "can be null or 'conversion.pdf'") String suffix, 
			@FormParam("expireHours") @ApiParam(value = "expiration time expressed in hours") Integer expireHours, 
			@FormParam("expireDate") @ApiParam(value = "exact expiration date expressed in the format yyyy-MM-dd") String expireDate, 
			@FormParam("maxDownloads") @ApiParam(value = "maximum number of downloads allowed", required = true) Integer maxDownloads) 
					throws Exception {
		String sid = validateSession();
		return super.createDownloadTicket(sid, docId, suffix, expireHours, expireDate, maxDownloads);
	}

	@Override
	@DELETE
	@Path("/deleteLink")
	@ApiOperation(value="Removes an existing link")
	public void deleteLink(@ApiParam(value = "ID of the link", required = true) @QueryParam("id") long id) throws Exception {
		String sid = validateSession();
		super.deleteLink(sid, id);
	}

	@Override
	@GET
	@Path("/getAliases")
	@ApiOperation(value="Gets the aliases", notes = "Gets the aliases of the given document; returns an array of WSDocument that are aliases")
	public WSDocument[] getAliases(@ApiParam(value = "The document ID", required = true) @QueryParam("docId") long docId) throws Exception {
		String sid = validateSession();
		return super.getAliases(sid, docId);
	}
	
	@Override
	@GET
	@Path("/getDocumentByCustomId")
	@ApiOperation(value="Gets document metadata by custom ID", notes = "Gets document metadata of an existing document with the given custom identifier")
	public WSDocument getDocumentByCustomId(@ApiParam(value = "The custom ID", required = true) @QueryParam("customId") String customId) throws Exception {
		String sid = validateSession();
		return super.getDocumentByCustomId(sid, customId);
	}

	@Override
	@GET
	@Path("/getDocuments")
	@ApiOperation(value="Gets the metadata of a collection of document", notes = "Gets document metadata of a collection of existing documents with the given identifiers; returns an array of WSDocument")	
	public WSDocument[] getDocuments(@QueryParam("docIds") @ApiParam(value = "Array of document IDs", required = true) Long[] docIds) throws Exception {
		String sid = validateSession();
		return super.getDocuments(sid, docIds);
	}

	@Override
	@GET
	@Path("/getExtractedText")
	@ApiOperation(value="Gets the extracted text of a document", notes = "Gets the document's text stored in the full-text index")	
	public String getExtractedText(@QueryParam("docId") @ApiParam(value = "The document ID", required = true) long docId) throws Exception {
		String sid = validateSession();
		return super.getExtractedText(sid, docId);
	}

	@Override
	@GET
	@Path("/getRecentDocuments")
	@ApiOperation(value="Gets the last modified documents", notes = "Lists of last modified documents of the current session")
	public WSDocument[] getRecentDocuments(@QueryParam("maxHits") @ApiParam(value = "Maximum number of returned records", required = true) Integer maxHits) throws Exception {
		String sid = validateSession();
		return super.getRecentDocuments(sid, maxHits);
	}

	@Override
	@GET
	@Path("/getLinks")
	@ApiOperation(value="Gets the links of a document", notes = "Gets all the links of a specific document; returns an array of links")
	public WSLink[] getLinks(@QueryParam("docId") @ApiParam(value = "The document ID", required = true) long docId) throws Exception {
		String sid = validateSession();
		return super.getLinks(sid, docId);
	}

	@Override
	@GET
	@Path("/getResource")
	@ApiOperation(value="Gets the content of a resource", notes = "Gets the content of a resource associated to the given document; returns the raw content of the file")	
	public DataHandler getResource(
			@QueryParam("docId") @ApiParam(value = "The document ID", required = true) long docId, 
			@QueryParam("fileVersion") @ApiParam(value = "The file version to retrieve") String fileVersion, 
			@QueryParam("suffix") @ApiParam(value = "suffix specification(it cannot be empty, use 'conversion.pdf' to get the PDF conversion)") String suffix) 
					throws Exception {
		String sid = validateSession();
		return super.getResource(sid, docId,fileVersion,suffix);
	}

	@Override
	@GET
	@Path("/isReadable")
	@ApiOperation(value="Tests if a document is readable", notes = "Tests if a document is readable; returns True if the identifier denotes a document, otherwise false")	
	public boolean isReadable(@QueryParam("docId") @ApiParam(value = "Document ID", required = true) long docId) throws Exception {
		String sid = validateSession();
		return super.isReadable(sid, docId);
	}

	@Override
	@POST
	@Path("/link")
	@ApiOperation(value="Creates a link between two documents", notes = "Creates a new link between two documents; returns the created link object")	
	public WSLink link(
			@FormParam("doc1") @ApiParam(value = "ID of document 1", required = true) long doc1, 
			@FormParam("doc2") @ApiParam(value = "ID of document 2", required = true) long doc2, 
			@FormParam("type") @ApiParam(value = "type of the link (use 'pdf' to point to the pdf conversion)") String type) 
					throws Exception {
		String sid = validateSession();
		return super.link(sid, doc1, doc2, type);
	}

	@Override
	@PUT
	@Path("/lock")
	@ApiOperation(value="Locks a document", notes = "Locks an existing document with the given identifier")	
	public void lock(@QueryParam("docId") @ApiParam(value = "Document ID", required = true) long docId) throws Exception {
		String sid = validateSession();
		super.lock(sid, docId);
	}

	@Override
	@POST
	@Path("/reindex")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@ApiOperation(value="Re-indexes a document", notes = "re-indexes(or indexes from scratch) a document")
	public void reindex(
			@FormParam("doc1") @ApiParam(value = "Document ID", required = true) long docId, 
			@FormParam("content") @ApiParam(value = "Document ID") String content) 
					throws Exception 
	{
		String sid = validateSession();
		super.reindex(sid, docId, content);
	}

	@Override
	@POST
	@Path("/uploadResource")
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@ApiOperation( value = "Uploads a new resource of the document", notes = "Uploads a new resource attached to the given document. If the resource already exists it is overwritten")
	@ApiImplicitParams({
			@ApiImplicitParam(name = "docId", value = "the document id", required = true, dataType = "integer", paramType = "form"),
			@ApiImplicitParam(name = "fileVersion", value = "the specific file version", required = false, dataType = "string", paramType = "form"),
			@ApiImplicitParam(name = "suffix", value = "suffix specification(it cannot be empty, use 'conversion.pdf' to put the PDF conversion)", required = true, dataType = "string", paramType = "form"),
			@ApiImplicitParam(name = "content", value = "raw content of the file", required = true, dataType = "file", paramType = "form"),
			})
	public void uploadResource(List<Attachment> attachments) throws Exception {

		String sid = validateSession();

		Long docId = null;
		String fileVersion = null;
		String suffix = null;
		DataHandler datah = null;

		for (Attachment att : attachments) {
			Map<String, String> params = att.getContentDisposition().getParameters();
			// log.debug("keys: {}", params.keySet());
			// log.debug("name: {}", params.get("name"));

			if ("docId".equals(params.get("name"))) {
				docId = Long.parseLong(att.getObject(String.class));
			} else if ("fileVersion".equals(params.get("fileVersion"))) {
				fileVersion = att.getObject(String.class);
			} else if ("suffix".equals(params.get("suffix"))) {
				suffix = att.getObject(String.class);
			} else if ("content".equals(params.get("content"))) {
				datah = att.getDataHandler();
			}
		}

		super.uploadResource(sid, docId, fileVersion, suffix, datah);
	}

	@Override
	@PUT
	@Path("/restore")
	@ApiOperation( value = "Restores a deleted document")
	public void restore(
			@QueryParam("docId") @ApiParam(value = "Document ID", required = true) long docId, 
			@QueryParam("folderId") @ApiParam(value = "Folder ID (target)", required = true) long folderId) throws Exception {
		String sid = validateSession();
		super.restore(sid, docId, folderId);
	}

	@Override
	@POST
	@Path("/saveNote")
	@ApiOperation( value = "Adds a new note", notes="Adds/modifies a note for the given document")
	public WSNote saveNote(
			@FormParam("docId") @ApiParam(value = "Document ID", required = true) long docId, 
			@FormParam("note") @ApiParam(value = "the WSNote representation as json string", required = true) WSNote note) throws Exception {
		String sid = validateSession();
		return super.saveNote(sid, docId, note);
	}

	@Override
	@POST
	@Path("/sendEmail")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@ApiOperation( value = "Sends documents by email", notes = "Sends a set of documents as mail attachments")	
	public void sendEmail(
			@FormParam("docIds") @ApiParam(value = "Document IDs", required = true) Long[] docIds, 
			@FormParam("recipients") @ApiParam(value = "Set of recipients(comma separated)", required = true) String recipients, 
			@FormParam("subject") @ApiParam(value = "The email subject") String subject, 
			@FormParam("message") @ApiParam(value = "The email message body") String message) 
					throws Exception {
		String sid = validateSession();
		super.sendEmail(sid, docIds, recipients, subject, message);
	}

	@Override
	@POST
	@Path("/setPassword")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@ApiOperation( value = "Password protect a document", notes = "Protects with a password the given document")		
	public void setPassword(
			@FormParam("docId") @ApiParam(value = "Document ID", required = true) long docId, 
			@FormParam("password") @ApiParam(value = "A password", required = true) String password) 
					throws Exception {
		String sid = validateSession();
		super.setPassword(sid, docId, password);
	}

	@Override
	@PUT
	@Path("/unlock")
	@ApiOperation( value = "Unlocks the document", notes = "Unlocks an existing document with the given identifier")			
	public void unlock(@QueryParam("docId") @ApiParam(value = "Document ID", required = true) long docId) 
			throws Exception {
		String sid = validateSession();
		super.unlock(sid, docId);
	}

	@Override
	@POST
	@Path("/unsetPassword")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@ApiOperation( value = "Removes password protection", notes = "Removes the password protection from the document")				
	public void unsetPassword(
			@FormParam("docId") @ApiParam(value = "Document ID", required = true) long docId, 
			@FormParam("currentPassword") @ApiParam(value = "A password", required = true) String currentPassword) 
					throws Exception {
		String sid = validateSession();
		super.unsetPassword(sid, docId, currentPassword);
	}

	@Override
	@POST
	@Path("/unprotect")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@ApiOperation( value = "Temporarily removes password protection", notes = "Unprotect a document that is password protected. If the given password is right, the document remains unprotected for the duration of the session")			
	public boolean unprotect(
			@FormParam("docId") @ApiParam(value = "Document ID", required = true) long docId, 
			@FormParam("password") @ApiParam(value = "A password", required = true) String password) 
					throws Exception {
		String sid = validateSession();
		return super.unprotect(sid, docId, password);
	}
}