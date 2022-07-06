package com.logicaldoc.core.searchengine;

import java.io.InputStream;
import java.util.Collection;

import com.logicaldoc.core.document.Document;

/**
 * Model of a search engine with search and I/O capabilities. Various
 * implementations can be provided, and the configuration property
 * <b>searchengine</b> must qualify the concrete implementation to use.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5
 */
public interface SearchEngine {

	/**
	 * Adds a new Hit into the index
	 * 
	 * @param document The document to add into the index
	 * @param content The extracted body text
	 * 
	 * @throws Exception is an error happens and the document cannot be added to
	 *         the index
	 */
	public void addHit(Document document, String content) throws Exception;

	/**
	 * Adds a new hit to the index
	 * 
	 * @param content Stream of the document's file
	 * @param document The document that we want to add
	 * 
	 * @throws Exception is an error happens and the document cannot be added to
	 *         the index
	 */
	public void addHit(Document document, InputStream content) throws Exception;

	/**
	 * Launch the index optimization that physically deletes the removed
	 * entries. This is a long running operation.
	 */
	public void optimize();

	/**
	 * Launch the check on the Index
	 * 
	 * @return the output of the check
	 */
	public String check();

	/**
	 * Deletes an hit in the index of the search engine.
	 * 
	 * @param id - id of the hit
	 */
	public void deleteHit(long id);

	/**
	 * Removed all passed hits from the index
	 * 
	 * @param ids Collection of hit identifiers
	 */
	public void deleteHits(Collection<Long> ids);

	public Hit getHit(long id);

	/**
	 * Search for hits.<br>
	 * Attention: The hits will be populated with just the fields stored in the
	 * index
	 * 
	 * @param expression the search expression
	 * @param filters a set of filter expressions, optional
	 * @param expressionLanguage the language in which the
	 *        <code>expression</code> is writted
	 * @param rows maximum number elements to be returned
	 * 
	 * @return the hits ordered by descending score
	 */
	public Hits search(String expression, String[] filters, String expressionLanguage, Integer rows);

	/**
	 * Executes a low-level query in the fulltext index<br>
	 * Attention: The hits will be populated with just the fields stored in the
	 * index
	 * 
	 * @param expression the query to use
	 * @param page the page to retrieve
	 * @param size the maximum number of entries to retrieve
	 * 
	 * @return the search result, ordered by ascending id
	 */
	public Hits query(String expression, int page, int size);

	
	/**
	 * Closes all indexing operations, shuts down the engine.
	 */
	public void close();

	/**
	 * This method can unlock a locked index.
	 */
	public void unlock();

	/**
	 * Check if at least one index is locked
	 * 
	 * @return true if one or more indexes are locked
	 */
	public boolean isLocked();

	/**
	 * Returns the number of indexed documents in all indexes. Used for
	 * statistical output.
	 * 
	 * @return the total number of entries in the index
	 */
	public long getCount();

	/**
	 * Drops the full-text index
	 */
	public void dropIndex();

	/**
	 * Removes from the index those entries without a coutnerpart in the
	 * database
	 * 
	 * @return number of deleted entries
	 */
	public long purge();

	/**
	 * To be called on the context startup, this method creates all indexes and
	 * unlock the existing ones
	 */
	public void init();

	/**
	 * Service method to get access from the internal core
	 * 
	 * @return the search server representation
	 */
	public Object getServer();
}