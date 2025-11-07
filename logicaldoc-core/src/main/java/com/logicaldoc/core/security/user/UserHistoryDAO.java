package com.logicaldoc.core.security.user;

import java.util.List;

import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.core.security.Client;
import com.logicaldoc.util.spring.Context;

/**
 * DAO for <code>UserHistory</code> handling.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.0
 */
public interface UserHistoryDAO extends PersistentObjectDAO<UserHistory> {

	/**
	 * Gets the object available in the application context
	 * 
	 * @return the instance of this object in the application context
	 */
	public static UserHistoryDAO get() {
		return Context.get(UserHistoryDAO.class);
	}
	
	/**
	 * This method selects all histories of a given user.
	 * 
	 * @param userId identifier of the suer
	 * 
	 * @return list of histories ordered by date asc
	 */
	public List<UserHistory> findByUserId(long userId);

	/**
	 * This method selects all histories of a given user and a given type.
	 * 
	 * @param userId identifier of the suer
	 * @param event The event specification to restrict the search (optional)
	 * 
	 * @return list of histories ordered by date asc
	 */
	public List<UserHistory> findByUserIdAndEvent(long userId, String event);

	/**
	 * Creates an user history entry
	 * 
	 * @param user The user that made the operation
	 * @param event The event specification
	 * @param comment The comment provided by the user
	 * @param client The client connected to LogicalDOC
	 * @param sessionId The user session id
	 * 
	 * @return the created history
	 */
	public UserHistory createUserHistory(User user, UserEvent event, String comment, String sessionId, Client client);

	/**
	 * This method deletes all the user history entries oldest than the given
	 * days from now. If <code>ttl</code> is 0 or -1, the cancellation is not
	 * made.
	 * 
	 * @param ttl The maximum number of days over which the history is
	 *        considered old
	 */
	public void cleanOldHistories(int ttl);
}
