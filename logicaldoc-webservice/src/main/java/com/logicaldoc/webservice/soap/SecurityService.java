package com.logicaldoc.webservice.soap;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.doc.WSDoc;
import com.logicaldoc.webservice.model.WSGroup;
import com.logicaldoc.webservice.model.WSUser;

/**
 * Security Web Service definition interface
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
@WSDoc(description = "users and groups handling")
@WebService(name = "Security", serviceName = "Security", targetNamespace = "http://ws.logicaldoc.com")
public interface SecurityService {

	/**
	 * Gets metadata of all existing users
	 * 
	 * @param sid identifier of the session, must be an administrator
	 * @param group name od the group
	 * 
	 * @return A value object containing the users metadata
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod
	@WebResult(name = "user")
	@WSDoc(description = "gets all existing users")
	public WSUser[] listUsers(
			@WSDoc(description = "identifier of the session, must be an administrator", required = true)
			@WebParam(name = "sid")
			String sid, @WSDoc(description = "if not null, all the users that belong to this group will be returned")
			@WebParam(name = "group")
			String group) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Gets group metadata of all existing groups.
	 * 
	 * @param sid identifier of the session, must be an administrator
	 * 
	 * @return A value object containing the groups metadata
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 */
	@WebMethod
	@WebResult(name = "group")
	@WSDoc(description = "gets all existing groups")
	public WSGroup[] listGroups(
			@WSDoc(description = "identifier of the session, must be an administrator", required = true)
			@WebParam(name = "sid")
			String sid) throws WebserviceException, PersistenceException;

	/**
	 * Creates/Updates a user. You can completely customize the user through a
	 * value object containing the user's metadata. The current user must be an
	 * administrator
	 * 
	 * @param sid identifier of the session, must be an administrator
	 * @param user Web service value object containing the user's metadata
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 */
	@WebResult(name = "userId")
	@WebMethod
	@WSDoc(description = "creates/updates a user; you can completely customize the user through a value object containing the user's metadata;"
			+ "<br/>the current user must be an administrator;"
			+ "<br/>returns the identifier of the created/updated user")
	public long storeUser(@WSDoc(description = "identifier of the session, must be an administrator", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "user")
	WSUser user) throws WebserviceException, PersistenceException;

	/**
	 * Creates/Updates a group. You can completely customize the group through a
	 * value object containing the group's metadata.
	 * 
	 * @param sid Session identifier. Must be an administrator.
	 * @param group Web service value object containing the group's metadata
	 * 
	 * @return id of the created/updated group.
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 */
	@WebResult(name = "groupId")
	@WebMethod
	@WSDoc(description = "creates/updates a group; you can completely customize the group through a value object containing the group's metadata;"
			+ "<br/>the current user must be an administrator;"
			+ "<br/>returns the identifier of the created/updated user")
	public long storeGroup(@WSDoc(description = "identifier of the session, must be an administrator", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "group")
	WSGroup group) throws WebserviceException, PersistenceException;

	/**
	 * Deletes an existing user with the given identifier
	 * 
	 * @param sid Session identifier. Must be an administrator
	 * @param userId The user id
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod
	@WSDoc(description = "deletes an existing user")
	public void deleteUser(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "userId")
	long userId) throws WebserviceException, PersistenceException, PermissionException;

	/**
	 * Deletes an existing group with the given identifier
	 * 
	 * @param sid Session identifier. Must be an administrator
	 * @param groupId The group id
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod
	@WSDoc(description = "deletes an existing group")
	public void deleteGroup(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "groupId")
	long groupId) throws PermissionException, PersistenceException, WebserviceException;

	/**
	 * changes the password of a user.<br>
	 * <b>0</b> if all is ok, <b>1</b> if the password is incorrect, <b>2</b> if
	 * the new password cannot be notified, otherwise a positive number grater
	 * than <b>2</b>
	 *
	 * @param sid identifier of the session
	 * @param userId The user Identifier. Must be an administrator.
	 * @param oldPassword can be null
	 * @param newPassword the new password
	 * 
	 * @return 0 if all is ok, 1 if the password is incorrect, 2 if the new
	 *         password cannot be notified, otherwise a positive number grater
	 *         than 2
	 *         
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 */
	@WebResult(name = "changeResult")
	@WebMethod
	@WSDoc(description = "changes the password of a user.<br/>"
			+ "<b>0</b> if all is ok, <b>1</b> if the password is incorrect, <b>2</b> if the new "
			+ "password cannot be notified, otherwise a positive number grater than <b>2</b>")
	public int changePassword(
			@WSDoc(description = "identifier of the session, must be an administrator", required = true)
			@WebParam(name = "sid")
			String sid, @WebParam(name = "userId")
			long userId, @WebParam(name = "oldPassword")
			String oldPassword, @WebParam(name = "newPassword")
			String newPassword) throws WebserviceException, PersistenceException;

	/**
	 * Gets user metadata of an existing user with the given identifier
	 * 
	 * @param sid Session identifier
	 * @param userId The user id
	 * 
	 * @return A value object containing the user's metadata
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 */
	@WebResult(name = "user")
	@WebMethod
	@WSDoc(description = "gets an existing user")
	public WSUser getUser(@WSDoc(description = "identifier of the session, must be an administrator", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "userId")
	long userId) throws WebserviceException, PersistenceException;

	/**
	 * Gets user metadata of an existing user with the given username
	 * 
	 * @param sid Session identifier
	 * @param username The user name
	 * 
	 * @return A value object containing the user's metadata
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 */
	@WebResult(name = "user")
	@WebMethod
	@WSDoc(description = "finds a user by his username")
	public WSUser getUserByUsername(
			@WSDoc(description = "identifier of the session, must be an administrator", required = true)
			@WebParam(name = "sid")
			String sid, @WebParam(name = "username")
			String username) throws WebserviceException, PersistenceException;

	/**
	 * Gets group metadata of an existing group with the given identifier
	 * 
	 * @param sid Session identifier
	 * @param groupId The group id
	 * 
	 * @return A value object containing the group's metadata
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 */
	@WebResult(name = "group")
	@WebMethod
	@WSDoc(description = "gets an existing group")
	public WSGroup getGroup(@WSDoc(description = "identifier of the session, must be an administrator", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "groupId")
	long groupId) throws WebserviceException, PersistenceException;
}