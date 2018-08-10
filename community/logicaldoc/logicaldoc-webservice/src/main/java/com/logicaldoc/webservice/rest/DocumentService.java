package com.logicaldoc.webservice.rest;

import java.util.List;

import javax.activation.DataHandler;
import javax.jws.WebMethod;
import javax.jws.WebParam;
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

import com.logicaldoc.webservice.doc.WSDoc;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSNote;
import com.logicaldoc.webservice.model.WSRating;

@Consumes({ MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON })
@Produces({ MediaType.APPLICATION_JSON })
public interface DocumentService {

	@POST
	@Path("/create")
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	// The "document" comes in the POST request body (encoded as JSON).
	Response create(List<Attachment> atts) throws Exception;

	@GET
	@Path("/getDocument")
	@Produces({ MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
	WSDocument getDocument(@QueryParam("docId") long docId) throws Exception;

	@POST
	@Path("/checkout")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	void checkout(@FormParam("docId") long docId) throws Exception;

	@POST
	@Path("/checkin")
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@Produces({ MediaType.TEXT_PLAIN, MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
	Response checkin(List<Attachment> attachments) throws Exception;

	@POST
	@Path("/upload")
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@Produces({ MediaType.TEXT_PLAIN, MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
	Response upload(List<Attachment> attachments) throws Exception;

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
	 * @throws Exception
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
	 */
	@POST
	@Path("/addNote")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED})
	public WSNote addNote(@FormParam("docId") long docId, @FormParam("note") String note) throws Exception;

	/**
	 * Deletes a new note by note identifier
	 */
	@DELETE
	@Path("/deleteNote")
	public void deleteNote(@QueryParam("noteId") long noteId) throws Exception;

	/**
	 * Gets the notes for the given document
	 */
	@GET
	@Path("/getNotes")
	public WSNote[] getNotes(@QueryParam("docId") long docId) throws Exception;

	/**
	 * Puts a new rating on the given document
	 */
	@GET
	@Path("/rateDocument")
	public WSRating rateDocument(@QueryParam("docId") long docId, @QueryParam("vote") int vote) throws Exception;

	/**
	 * Gets all the ratings of the given document
	 */
	@GET
	@Path("/getRatings")
	public WSRating[] getRatings(@QueryParam("docId") long docId) throws Exception;
	
	/**
	 * Deletes a version by document identifier and version ID. You can not delete the latest version of a document
	 */
	@DELETE
	@Path("/deleteVersion")
	public String deleteVersion(@QueryParam("docId") long docId, @QueryParam("version") String version) throws Exception;
	
	/**
	 * Moves an existing document with the given identifier.
	 * 
	 * @param docId The document id
	 * @param folderId Identifier of the new document's folder
	 */
	@PUT
	@Path("/move")
	public void move(@QueryParam("docId") long docId, @QueryParam("folderId") long folderId) throws Exception;
	
	
	/**
	 * Creates the thumbail of the given document; if the thumbnail was already created, nothing will happen
	 * 
	 * @param docId The document id
	 * @param fileVersion The specific file version(it can be empty)
	 * @throws Exception
	 */
	@PUT
	@Path("/createThumbnail")
	public void createThumbnail(
			@QueryParam("docId") long docId,
			@QueryParam("fileVersion") String fileVersion)	throws Exception;
	
	/**
	 * Creates the PDF conversion of the given document; if the PDF conversion was already created, nothing will happen
	 * 
	 * @param docId The document id
	 * @param fileVersion The specific file version(it can be empty)
	 * @throws Exception
	 */
	@PUT
	@Path("/createPdf")
	public void createPdf(
			@QueryParam("docId") long docId,
			@QueryParam("fileVersion") String fileVersion)	throws Exception;
}