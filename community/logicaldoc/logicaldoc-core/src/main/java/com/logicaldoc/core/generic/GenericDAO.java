package com.logicaldoc.core.generic;

import java.util.List;

import com.logicaldoc.core.PersistentObjectDAO;

/**
 * Instances of this class is a DAO-service for Generic business entities.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
public interface GenericDAO extends PersistentObjectDAO<Generic> {
	/**
	 * Finds a Generic by it's alternate key
	 * 
     * @param type The type(you can use like jollies and can be null)
	 * @param subtype The sub-type(you can use like jollies and can be null)
	 * @param tenantId ID of the owning tenant (optional)
	 * @param qualifier the qualifier, can be null
	 * 
	 * @return Wanted generic or null
	 */
	public Generic findByAlternateKey(String type, String subtype, Long qualifier, long tenantId);

	/**
	 * Finds a Generic by it's alternate key. The search uses the like operator
	 * and each parameter can be null.
	 * 
	 * @param type The type(you can use like jollies and can be null)
	 * @param subtype The sub-type(you can use like jollies and can be null)
	 * @param tenantId ID of the owning tenant (optional)
	 * @param qualifier the qualifier, can be null
	 * 
	 * @return The collection of found Generics
	 */
	public List<Generic> findByTypeAndSubtype(String type, String subtype, Long qualifier, Long tenantId);
}
