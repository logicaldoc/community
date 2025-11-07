package com.logicaldoc.core.document;

import java.util.Collection;
import java.util.List;
import java.util.Set;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.util.spring.Context;

/**
 * DAO for <code>DocumentNote</code> handling.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.2
 */
public interface DocumentNoteDAO extends PersistentObjectDAO<DocumentNote> {

	/**
	 * Gets the object available in the application context
	 * 
	 * @return the instance of this object in the application context
	 */
	public static DocumentNoteDAO get() {
		return Context.get(DocumentNoteDAO.class);
	}
	
	/**
	 * Stores a note and saves the document's history
	 * 
	 * @param note the note
	 * @param transaction session informations
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public void store(DocumentNote note, DocumentHistory transaction) throws PersistenceException;

	/**
	 * This method finds the list of document notes regarding a document with
	 * the given ID
	 * 
	 * @param docId ID of the document
	 * @param userId identifier of the current user
	 * @param fileVersion indicates a specific file version, optional
	 * 
	 * @return The list of document note
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<DocumentNote> findByDocId(long docId, long userId, String fileVersion) throws PersistenceException;

	/**
	 * This method finds the list of document notes regarding a document with
	 * the given ID and optionally filter on the type
	 * 
	 * @param docId ID of the document
	 * @param userId identifier of the current user
	 * @param fileVersion indicates a specific file version, optional
	 * @param type note type, optional
	 * 
	 * @return The list of document note
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<DocumentNote> findByDocIdAndType(long docId, long userId, String fileVersion, String type)
			throws PersistenceException;

	/**
	 * This method finds the list of document notes regarding a document with
	 * the given ID and optionally filter on a collection of types
	 * 
	 * @param docId ID of the document
	 * @param userId identifier of the current user
	 * @param fileVersion indicates a specific file version, optional
	 * @param types collection of admitted note types, optional
	 * 
	 * @return The list of document note
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<DocumentNote> findByDocIdAndTypes(long docId, long userId, String fileVersion, Collection<String> types)
			throws PersistenceException;

	/**
	 * Copies all the notes not associated to a specific page from a given file
	 * version to another
	 * 
	 * @param docId The document ID
	 * @param oldFileVersion the old version
	 * @param newFileVersion the version to copy to
	 *
	 * @return Number of copied notes
	 * 
	 * @throws PersistenceException If an error occurs in the database
	 */
	public int copyAnnotations(long docId, String oldFileVersion, String newFileVersion) throws PersistenceException;

	/**
	 * This method finds the list of document notes regarding posted by a
	 * specific user
	 * 
	 * @param userId ID of the user
	 * 
	 * @return The list of document notes ordered by descending date
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<DocumentNote> findByUserId(long userId) throws PersistenceException;

	/**
	 * This method is looking up for writing rights for a note and an user
	 * 
	 * @param noteId ID of the note
	 * @param userId ID of the user
	 * 
	 * @return id the user has write permission
	 * 
	 * @throws PersistenceException Error in the database 
	 */
	public boolean isWriteAllowed(long noteId, long userId) throws PersistenceException;

	/**
	 * This method is looking up for read rights for a note and an user
	 * 
	 * @param noteId ID of the note
	 * @param userId ID of the user
	 * 
	 * @return if the user can access the note
	 * 
	 * @throws PersistenceException Error in the database  
	 */
	public boolean isReadAllowed(long noteId, long userId) throws PersistenceException;
	
	/**
	 * Finds all permissions of a user enabled on the specified note
	 * 
	 * @param noteId ID of the note
	 * @param userId ID of the user
	 * 
	 * @return Collection of enabled permissions
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public Set<Permission> getAllowedPermissions(long noteId, long userId) throws PersistenceException;
}