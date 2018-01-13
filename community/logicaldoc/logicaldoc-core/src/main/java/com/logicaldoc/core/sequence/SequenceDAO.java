package com.logicaldoc.core.sequence;

import java.util.List;

import com.logicaldoc.core.PersistentObjectDAO;

/**
 * Utility DAO that can manage sequences persisted on the DB
 * <p>
 * <b>Important:</b> Implementations of this interface must grant
 * synchronization.
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 4.0
 */
public interface SequenceDAO extends PersistentObjectDAO<Sequence> {

	/**
	 * Returns the next value of the sequence
	 * 
	 * @param sequence The sequence name
	 * @param objectId The sequence object ID
	 * @param tenantId ID of the owning tenant
	 * 
	 * @return The next value
	 */
	public long next(String sequence, long objectId, long tenantId);

	/**
	 * Returns the next value of the sequence by incrementing by the given
	 * increment
	 * 
	 * @param sequence The sequence name
	 * @param objectId The sequence object ID
	 * @param tenantId ID of the owning tenant
	 * @param increment ID of the owning tenant
	 * 
	 * @return The next value
	 */
	public long next(String sequence, long objectId, long tenantId, long increment);

	/**
	 * Initializes the sequence value
	 * 
	 * @param sequence The sequence name
	 * @param objectId The sequence object ID
	 * @param value The value
	 * @param tenantId ID of the owning tenant
	 * 
	 * @param value The initial value
	 */
	public void reset(String sequence, long objectId, long tenantId, long value);

	/**
	 * Deletes the sequence.
	 */
	public void delete(String sequence, long objectId, long tenantId);

	/**
	 * Finds all sequences whose name starts with the passed name
	 */
	public List<Sequence> findByName(String name, long tenantId);

	/**
	 * Finds the sequence by the given alternate key
	 */
	public Sequence findByAlternateKey(String name, long objectId, long tenantId);

	/**
	 * Gets the current value
	 */
	public long getCurrentValue(String sequence, long objectId, long tenantId);
}