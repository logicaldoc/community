package com.logicaldoc.core.document;

import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.util.spring.Context;

/**
 * DAO service for ratings
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public interface RatingDAO extends PersistentObjectDAO<Rating> {

	/**
	 * Gets the object loaded in the execution context
	 * 
	 * @return the instance of this object in the execution context
	 */
	public static RatingDAO get() {
		return Context.get(RatingDAO.class);
	}
	
	/**
	 * Stores a rating and saves the document's history
	 * 
	 * @param rating the rating
	 * @param transaction session informations
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public void store(Rating rating, DocumentHistory transaction) throws PersistenceException;

	/**
	 * Updates the document's rating with the votes average
	 * 
	 * @param docId Identifier of the document
	 * @param transaction session informations
	 * 
	 * @return the new rating
	 * 
	 * @throws PersistenceException raised in case of database error
	 */
	public int updateDocumentRating(long docId, DocumentHistory transaction) throws PersistenceException;

	/**
	 * Returns a rating that contains count and average of vote on the given
	 * document
	 * 
	 * @param docId ID of the document
	 * 
	 * @return the rating for the document
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public Rating findVotesByDocId(long docId) throws PersistenceException;

	/**
	 * Finds the rating for the given user id and the given document id
	 * 
	 * @param docId ID of the document
	 * @param userId ID of the user
	 * 
	 * @return the vote of the given user on the document
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public Rating findByDocIdAndUserId(long docId, long userId) throws PersistenceException;

	/**
	 * Finds the ratings stored for the given document id
	 * 
	 * @param docId ID of the document
	 * 
	 * @return the list of ratings
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<Rating> findByDocId(long docId) throws PersistenceException;
}