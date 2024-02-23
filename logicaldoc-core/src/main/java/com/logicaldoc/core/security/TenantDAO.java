package com.logicaldoc.core.security;

import java.util.Set;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;

public interface TenantDAO extends PersistentObjectDAO<Tenant> {

	/**
	 * Finds a tenant by name
	 * 
	 * @param name name of wanted tenant
	 * 
	 * @return Wanted tenant or null
	 */
	public Tenant findByName(String name);

	/**
	 * Retrieves the tenant's name
	 *
	 * @param tenantId identifier of the tenant
	 * 
	 * @return name of the tenant
	 */
	public String getTenantName(long tenantId);

	/**
	 * Retrieves all the tenant names
	 * 
	 * @return the collection of all the tenant names
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public Set<String> findAllNames() throws PersistenceException;

	/**
	 * Counts the total number of tenants
	 * 
	 * @return number of tenants
	 */
	public int count();
}