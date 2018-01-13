package com.logicaldoc.webservice.soap;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;

import com.logicaldoc.webservice.doc.WSDoc;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSTagCloud;

/**
 * Tag Web Service definition interface
 * 
 * Marco Meschieri - LogicalDOC
 * 
 * @since 7.6.3
 */
@WSDoc(description = "Tags management and search")
@WebService(name = "Tag", serviceName = "Tag", targetNamespace = "http://ws.logicaldoc.com")
public interface TagService {

	/**
	 * Sets the tags of a document
	 * 
	 * @param sid Session Identifier
	 */
	@WebMethod
	@WSDoc(description = "sets the tags of a document")
	public void setDocumentTags(@WebParam(name = "sid") String sid, @WebParam(name = "docId") long docId,
			@WebParam(name = "tag") String[] tags) throws Exception;

	/**
	 * Adds tags to a document
	 * 
	 * @param sid Session Identifier
	 */
	@WebMethod
	@WSDoc(description = "adds tags to a document")
	public void addDocumentTags(@WebParam(name = "sid") String sid, @WebParam(name = "docId") long docId,
			@WebParam(name = "tag") String[] tags) throws Exception;

	/**
	 * Retrieves all the tags of a document.
	 * 
	 * @param sid Session Identifier
	 * @return The tags of the document
	 */
	@WebMethod
	@WebResult(name = "tag")
	@WSDoc(description = "retrieves all the tags of a document")
	public String[] getDocumentTags(@WebParam(name = "sid") String sid, @WebParam(name = "docId") long docId)
			throws Exception;

	/**
	 * Sets the tags of a folder
	 * 
	 * @param sid Session Identifier
	 */
	@WebMethod
	@WSDoc(description = "sets the tags of a folder")
	public void setFolderTags(@WebParam(name = "sid") String sid, @WebParam(name = "folderId") long folderId,
			@WebParam(name = "tag") String[] tags) throws Exception;

	/**
	 * Adds tags to a folder
	 * 
	 * @param sid Session Identifier
	 */
	@WebMethod
	@WSDoc(description = "adds tags to a folder")
	public void addFolderTags(@WebParam(name = "sid") String sid, @WebParam(name = "folderId") long folderId,
			@WebParam(name = "tag") String[] tags) throws Exception;

	/**
	 * Retrieves all the tags of a folder.
	 * 
	 * @param sid Session Identifier
	 * @return The tags of the folder
	 */
	@WebMethod
	@WebResult(name = "tag")
	@WSDoc(description = "retrieves all the tags of a folder")
	public String[] getFolderTags(@WebParam(name = "sid") String sid, @WebParam(name = "folderId") long folderId)
			throws Exception;

	/**
	 * Retrieves all the tags in the repository.
	 * 
	 * @param sid Session Identifier
	 * @return The tags in the repository
	 */
	@WebMethod
	@WebResult(name = "tag")
	@WSDoc(description = "retrieves all the tags in the repository")
	public String[] getTags(@WebParam(name = "sid") String sid) throws Exception;

	/**
	 * Retrieves all tag clouds in the repository.
	 * 
	 * @param sid Session Identifier
	 * @return The tags in the repository
	 */
	@WebMethod
	@WebResult(name = "tagCloud")
	@WSDoc(description = "retrieves all tag clouds in the repository")
	public WSTagCloud[] getTagCloud(@WebParam(name = "sid") String sid) throws Exception;

	/**
	 * Finds authorized documents for the current user having a specified tag.
	 * 
	 * @param tag Tag of the document
	 * @return Collection of found documents.
	 */
	@WebMethod
	@WebResult(name = "document")
	@WSDoc(description = "finds authorized documents for the current user having a specified tag")
	public WSDocument[] findDocumentsByTag(@WebParam(name = "sid") String sid, @WebParam(name = "tag") String tag)
			throws Exception;

	/**
	 * Finds authorized folders for the current user having a specified tag.
	 * 
	 * @param tag Tag of the folder
	 * @return Collection of found folders.
	 */
	@WebMethod
	@WebResult(name = "folder")
	@WSDoc(description = "finds authorized folders for the current user having a specified tag")
	public WSFolder[] findFoldersByTag(@WebParam(name = "sid") String sid, @WebParam(name = "tag") String tag)
			throws Exception;
}
