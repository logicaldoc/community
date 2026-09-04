package com.logicaldoc.core.ticket;

import java.util.Map;

import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.util.spring.Context;

/**
 * DAO for {@link Ticket}s handling.
 * 
 * @author Michael Scholz
 * @author Marco Meschieri
 */
public interface TicketDAO extends PersistentObjectDAO<Ticket> {

    /**
     * Gets the object available in the application context
     * 
     * @return the instance of this object in the application context
     */
    public static TicketDAO get() {
        return Context.get(TicketDAO.class);
    }

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
     * This finds a ticket by its identifier and runs the validation
     * 
     * @param ticketId The ticket id
     * 
     * @return Ticket with given ticket id
     * 
     * @throws PersistenceException error at database level
     */
    public Ticket findByTicketId(String ticketId) throws PersistenceException;

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

    /**
     * Counts the DOWNLOAD or VIEW tickets
     * 
     * @param tenantId optional tenant specification
     * 
     * @return Total count
     * 
     * @throws PersistenceException error in the database
     */
    public long countViewOrDownloadTickets(Long tenantId) throws PersistenceException;

    /**
     * Just checks if a given ID refers to a support ticket
     * 
     * @param ticketId the ticket
     * 
     * @return If the ticketId is a support ticket
     */
    default public boolean isSupportTicket(String ticketId) {
        try {
            return TicketDAO.get().queryForInt(
                    "select count(*) from ld_ticket where ld_enabled = 1 and ld_deleted = 0 and ld_type = :type and ld_ticketid = :id",
                    Map.of("type", Ticket.SUPPORT, "id", ticketId)) > 0;
        } catch (PersistenceException e) {
            LoggerFactory.getLogger(TicketDAO.class).warn(e.getMessage(), e);
            return false;
        }
    }
}