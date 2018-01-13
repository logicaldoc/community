package com.logicaldoc.core.contact;

import java.util.List;

import com.logicaldoc.core.PersistentObjectDAO;

/**
 * Instances of this class is a DAO-service for Contact business entities.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.8
 */
public interface ContactDAO extends PersistentObjectDAO<Contact> {

	/**
	 * Finds the contacts of the specified user.
	 * 
	 * @param userId The ID of the user or null
	 * @param email Optional email specification,
	 * 
	 * @return The collection of found Contacts
	 */
	public List<Contact> findByUser(Long userId, String email);
}
