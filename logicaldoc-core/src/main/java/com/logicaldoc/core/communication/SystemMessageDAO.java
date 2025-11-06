package com.logicaldoc.core.communication;

import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.util.spring.Context;

/**
 * This is a DAO service for SystemMessages.
 * 
 * @author Michael Scholz
 * @author Marco Meschieri
 * @version 1.0
 */
public interface SystemMessageDAO extends PersistentObjectDAO<SystemMessage> {

	/**
	 * Gets the object loaded in the execution context
	 * 
	 * @return the instance of this object in the execution context
	 */
	public static SystemMessageDAO get() {
		return Context.get(SystemMessageDAO.class);
	}

	/**
	 * This method selects all the messages for the specified recipient and type
	 * 
	 * @param recipient The recipient name
	 * @param type The message type
	 * @param read Optional flag
	 * 
	 * @return The messages list
	 * 
	 * @throws PersistenceException @throws PersistenceException Error in the
	 *         database
	 */
	public List<SystemMessage> findByRecipient(String recipient, int type, Integer read) throws PersistenceException;

	/**
	 * This methods gets the number of unread messages for the specified
	 * recipient and type.
	 * 
	 * @param recipient The recipient name
	 * @param type The message type
	 * 
	 * @return The number of messages
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public int getUnreadCount(String recipient, int type) throws PersistenceException;

	/**
	 * Removes all system expired messages for the specified recipient
	 * 
	 * @param recipient The recipient
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public void deleteExpiredMessages(String recipient) throws PersistenceException;

	/**
	 * Removes all expired messages for the specified type
	 * 
	 * @param type The message type
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public void deleteExpiredMessages(int type) throws PersistenceException;

	/**
	 * This method selects all the messages for the specified type
	 * 
	 * @param type type of the message
	 * @return The list of messages with the given type
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<SystemMessage> findByType(int type) throws PersistenceException;

	/**
	 * This method selects all the messages for the specified mode
	 * 
	 * @param mode The message mode
	 * @return The list of messages of the given mode
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<SystemMessage> findByMode(String mode) throws PersistenceException;

	/**
	 * This method selects all the messages for the specified type that are not
	 * been already sent and for which the number of sending trials is less than
	 * the maximum number (parameter 'notifier.maxtrials')
	 * 
	 * @param type The message type
	 * @param maxTrials The maximum number of sending trials
	 * 
	 * @return The list of messages of the given type
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<SystemMessage> findMessagesToBeSent(int type, int maxTrials) throws PersistenceException;
}