package com.logicaldoc.core.ticket;

import java.util.Map;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.DocumentHistory;

/**
 * This interface defines hooks called before and after a particular event
 * occurs on the specified ticket.
 * <p>
 * Each methods has access to a dictionary map that can be used through the
 * execution pipeline in order to carry needed informations among all listeners.
 * </p>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.3
 */
public interface TicketListener {

	/**
	 * Called before a ticket is stored in the database
	 * 
	 * @param ticket The ticket to be stored
	 * @param transaction Transaction informations
	 * @param dictionary Dictionary of the execution pipeline
	 * 
	 * @throws PersistenceException raises if some kind of error happens during
	 *         execution
	 */
	public void beforeStore(Ticket ticket, DocumentHistory transaction, Map<String, Object> dictionary)
			throws PersistenceException;

	/**
	 * Called after a ticket is stored in the database
	 * 
	 * @param ticket The ticket to be stored
	 * @param transaction Transaction informations
	 * @param dictionary Dictionary of the execution pipeline
	 * 
	 * @throws PersistenceException raises if some kind of error happens during
	 *         execution
	 */
	public void afterStore(Ticket ticket, DocumentHistory transaction, Map<String, Object> dictionary)
			throws PersistenceException;

	/**
	 * Called to perform required validations on a ticket
	 * 
	 * @param ticket The ticket to be evaluated
	 * @param dictionary Dictionary of the execution pipeline
	 * 
	 * @throws PersistenceException raises if some kind of error happens during
	 *         execution
	 */
	public void validate(Ticket ticket, Map<String, Object> dictionary) throws PersistenceException;
}