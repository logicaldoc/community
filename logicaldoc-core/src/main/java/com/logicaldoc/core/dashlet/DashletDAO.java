package com.logicaldoc.core.dashlet;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;

/**
 * Instances of this class is a DAO-service for dashlet objects.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @version 8.2.3
 */
public interface DashletDAO extends PersistentObjectDAO<Dashlet> {

	/**
	 * Finds the dashlet by it's name
	 * 
	 * @param name name of the dashlet
	 * @param tenantId identifier of the tenant
	 * 
	 * @return the found dashlet
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public Dashlet findByName(String name, long tenantId) throws PersistenceException;
}