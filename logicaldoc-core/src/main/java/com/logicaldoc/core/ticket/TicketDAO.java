package com.logicaldoc.core.ticket;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.core.document.DocumentHistory;

/**
 * DAO for <code>Ticket</code> handling.
 * 
 * @author Michael Scholz
 * @author Marco Meschieri
 */
public interface TicketDAO extends PersistentObjectDAO<Ticket> {

	/**
	 * This method deletes a download ticket.
	 * 
	 * @param ticketId ID of the ticket which should be delete.
	 * 
	 * @return if the tickets have been deleted
	 */
	public boolean deleteByTicketId(String ticketId);

	/**
	 * This method deletes all tickets of the specified document.
	 * 
	 * @param docId ID of the document
	 * 
	 * @return if the ticket has been deleted
	 */
	public boolean deleteByDocId(long docId);

	/**
	 * Deletes all expired tickets
	 */
	public void deleteExpired();

	/**
	 * This finds a ticket by its identifier
	 * 
	 * @param ticketId The ticket id
	 * 
	 * @return Ticket with given ticket id
	 */
	public Ticket findByTicketId(String ticketId);

	/**
	 * This method persists the download ticket object and insert a new document
	 * history entry
	 * 
	 * @param ticket the ticket to store
	 * @param transaction entry to log the event
	 * 
	 * @throws PersistenceException error at database level
	 */
	public void store(Ticket ticket, DocumentHistory transaction) throws PersistenceException;
}