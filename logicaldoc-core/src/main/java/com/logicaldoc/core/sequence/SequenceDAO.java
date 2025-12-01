package com.logicaldoc.core.sequence;

import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.util.spring.Context;

/**
 * Utility DAO that can manage sequences persisted in the DB
 * <p>
 * <b>Important:</b> Implementations of this interface must grant
 * synchronization.
 * </p>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 4.0
 */
public interface SequenceDAO extends PersistentObjectDAO<Sequence> {

	/**
	 * Gets the object available in the application context
	 * 
	 * @return the instance of this object in the application context
	 */
	public static SequenceDAO get() {
		return Context.get(SequenceDAO.class);
	}

	/**
	 * Returns the next value of the sequence
	 * 
	 * @param name name of the sequence
	 * @param objectId The sequence object ID
	 * @param tenantId ID of the owning tenant
	 * 
	 * @return The next value
	 * 
	 * @throws PersistenceException an error happened in the database
	 */
	public long next(String name, long objectId, long tenantId) throws PersistenceException;

	/**
	 * Returns the next value of the sequence by incrementing by the given
	 * increment
	 * 
	 * @param name name of the sequence
	 * @param objectId The sequence object ID
	 * @param tenantId ID of the owning tenant
	 * @param increment ID of the owning tenant
	 * 
	 * @return The next value
	 * 
	 * @throws PersistenceException an error happened in the database 
	 */
	public long next(String name, long objectId, long tenantId, long increment) throws PersistenceException;

	/**
	 * Initializes the sequence value
	 * 
	 * @param name name of the sequence
	 * @param objectId The sequence object ID
	 * @param value The initial value
	 * @param tenantId ID of the owning tenant
	 * 
	 * @throws PersistenceException an error happened in the database
	 */
	public void reset(String name, long objectId, long tenantId, long value) throws PersistenceException;

	/**
	 * Deletes the sequence
	 * 
	 * @param name name of the sequence
	 * @param objectId value for the <b>objectId</b> field
	 * @param tenantId identifier of the tenant
	 * 
	 * @throws PersistenceException if a database error occurs
	 */
	public void delete(String name, long objectId, long tenantId) throws PersistenceException;

	/**
	 * Finds all sequences whose name starts with the passed name
	 * 
	 * @param name name of the sequence
	 * @param tenantId identifier of the tenant
	 * 
	 * @return collection of sequences
	 */
	public List<Sequence> findByName(String name, long tenantId) throws PersistenceException;

	/**
	 * Finds the sequence by the given alternate key
	 * 
	 * @param name name of the sequence
	 * @param objectId value for the <b>objectId</b> field
	 * @param tenantId identifier of the tenant
	 * 
	 * @return the sequence object
	 */
	public Sequence findByAlternateKey(String name, long objectId, long tenantId) throws PersistenceException;

	/**
	 * Gets the current value
	 * 
	 * @param name name of the sequence
	 * @param objectId value for the <b>objectId</b> field
	 * @param tenantId identifier of the tenant
	 * 
	 * @return current value of the sequence
	 * 
	 * @throws PersistenceException an error happened in the database
	 */
	public long getCurrentValue(String name, long objectId, long tenantId) throws PersistenceException;
}