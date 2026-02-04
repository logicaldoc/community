package com.logicaldoc.core.filler;

import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.util.spring.Context;

/**
 * This interface is a DAO-service for {@link Filler}s
 * 
 * @author Marco Meschieri - LogicalDOC
 * @version 9.2.3
 */
public interface FillerDAO extends PersistentObjectDAO<Filler> {

	/**
	 * Gets the object available in the application context
	 * 
	 * @return the instance of this object in the application context
	 */
	public static FillerDAO get() {
		return Context.get(FillerDAO.class);
	}

	/**
	 * Finds the filler by it's name
	 * 
	 * @param name The name
	 * @param tenantId Identifier of the tenant
	 * 
	 * @return The found filler
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public Filler findByName(String name, long tenantId) throws PersistenceException;

	/**
	 * Finds the fillers by type
	 * 
	 * @param type Type of filler
	 * @param tenantId Identifier of the tenant
	 * 
	 * @return The list of fillers of same type ordered by name
	 * 
	 * @throws PersistenceException Error in the database
	 * 
	 * @param <S> what filler type to retrieve
	 */
	public <S extends Filler> List<S> findByType(Class<S> type, long tenantId) throws PersistenceException;
}