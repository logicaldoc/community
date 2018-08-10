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
	 * @param document
	 * @param content
	 * @return
	 * @throws Exception
	 */
	public void addHit(Document document, String content) throws Exception;

	/**
	 * Adds a new hit to the index
	 * 
	 * @param content Stream of the document's file
	 * @param document The document that we want to add
	 * @throws Exception
	 */
	public void addHit(Document document, InputStream content) throws Exception;

	/**
	 * Launch the index optimization and triggers the build of spellcheck
	 * dictionary
	 */
	public void optimize();

	/**
	 * Launch the check on the Index
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
	 * index.
	 */
	public Hits search(String expression, String[] filters, String expressionLanguage, Integer rows);

	/**
	 * Close all indexing operations, shuts down the engine.
	 */
	public void close();

	/**
	 * This method can unlock a locked index.
	 */
	public void unlock();

	/**
	 * 
	 * Check if at least one index is locked
	 * 
	 * @return true if one or more indexes are locked
	 */
	public boolean isLocked();

	/**
	 * Returns the number of indexed documents in all indexes. Used for
	 * statistical output.
	 */
	public long getCount();

	/**
	 * Drops the fulltext index
	 */
	public void dropIndex();

	/**
	 * To be called on the context startup, this method creates all indexes and
	 * unlock the existing ones
	 */
	public void init();

	/**
	 * Service method to get access from the internal core
	 */
	public Object getServer();
}