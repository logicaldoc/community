package com.logicaldoc.core.document;

import java.util.List;

import com.logicaldoc.core.PersistentObjectDAO;

/**
 * This class is a DAO-service for versions.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @version 4.5
 */
public interface VersionDAO extends PersistentObjectDAO<Version> {
	/**
	 * This method finds a version by the document's ID an the version code.
	 * 
	 * @param docId ID of the document
	 * @param version the version code
	 * @return the found version
	 */
	public Version findByVersion(long docId, String version);

	/**
	 * This method finds a the first version with the given fileVersion
	 * 
	 * @param docId ID of the document
	 * @param fileVersion the fileVersion code
	 * @return the found version
	 */
	public Version findByFileVersion(long docId, String fileVersion);
	
	/**
	 * Finds all versions of the given document
	 * 
	 * @param docId The document's id
	 * @return The list of versions ordered by descending date
	 */
	public List<Version> findByDocId(long docId);


	/**
	 * Updates the version's digest (SHA-1)
	 * 
	 * @param version The version to be processed
	 */
	public void updateDigest(Version version);
}