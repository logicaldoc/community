package com.logicaldoc.core.ticket;

import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.core.document.History;

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
	 * @param historyid ID of the ticket which should be delete.
	 */
	public boolean deleteByTicketId(String ticketId);

	/**
	 * This method deletes all tickets of the specified document.
	 * 
	 * @param docId ID of the document
	 */
	public boolean deleteByDocId(long docId);

	/**
	 * Deletes all expired tickets
	 */
	public void deleteExpired();

	/**
	 * This finds a ticket by its identifier.
	 * 
	 * @param ticketId The ticket id
	 * @return Ticket with given ticket id.
	 */
	public Ticket findByTicketId(String ticketId);
	
	/**
	 * This method persists the download ticket object and insert a new document
	 * history entry.
	 * 
	 * @param ticket
	 * @param transaction entry to log the event
	 * @return True if successfully stored in a database.
	 */
	public boolean store(Ticket ticket,  History transaction);
}