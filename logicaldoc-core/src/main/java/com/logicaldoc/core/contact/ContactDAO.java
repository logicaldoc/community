package com.logicaldoc.core.contact;

import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.util.spring.Context;

/**
 * Instances of this class is a DAO-service for Contact business entities.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.8
 */
public interface ContactDAO extends PersistentObjectDAO<Contact> {

	/**
	 * Gets the object available in the application context
	 * 
	 * @return the instance of this object in the application context
	 */
	public static ContactDAO get() {
		return Context.get(ContactDAO.class);
	}

	/**
	 * Finds the contacts of the specified user.
	 * 
	 * @param userId The ID of the user or null
	 * @param email Optional email specification,
	 * 
	 * @return The collection of found Contacts
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<Contact> findByUser(Long userId, String email) throws PersistenceException;
}
