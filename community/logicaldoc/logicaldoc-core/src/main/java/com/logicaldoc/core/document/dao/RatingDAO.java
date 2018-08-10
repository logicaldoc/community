package com.logicaldoc.core.document.dao;

import java.util.List;

import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.core.document.History;
import com.logicaldoc.core.document.Rating;

/**
 * DAO service for ratings.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public interface RatingDAO extends PersistentObjectDAO<Rating> {

	/**
	 * Stores a rating and saves the document's history
	 */
	public boolean store(Rating rating, History transaction);

	/**
	 * Returns a rating that contains count and average of vote on the given
	 * document.
	 * 
	 * @param docId ID of the document.
	 * @return Number of ratings on the given document.
	 */
	public Rating findVotesByDocId(long docId);

	/**
	 * Finds the rating for the given user id and the given document id.
	 * 
	 * @param docId ID of the document.
	 * @param userId ID of the user.
	 * @return the vote of the given user on the document
	 */
	public Rating findByDocIdAndUserId(long docId, long userId);

	/**
	 * Finds the ratings stored for the given document id.
	 * 
	 * @param docId ID of the document
	 * @return the list of ratings
	 */
	public List<Rating> findByDocId(long docId);
}