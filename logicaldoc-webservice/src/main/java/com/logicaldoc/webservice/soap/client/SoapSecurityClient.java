package com.logicaldoc.webservice.soap.client;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSGroup;
import com.logicaldoc.webservice.model.WSUser;
import com.logicaldoc.webservice.soap.SecurityService;

/**
 * Security Web Service client.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class SoapSecurityClient extends SoapClient<SecurityService> implements SecurityService {

	public SoapSecurityClient(String endpoint) {
		super(endpoint, SecurityService.class, -1, true, -1);
	}

	@Override
	public List<WSUser> listUsers(String sid, String group)
			throws AuthenticationException, WebserviceException, PersistenceException {
		final List<WSUser> users = client.listUsers(sid, group);
		if (users != null)
			return users;
		else
			return new ArrayList<>();
	}

	@Override
	public List<WSGroup> listGroups(String sid) throws WebserviceException, PersistenceException {
		final List<WSGroup> groups = client.listGroups(sid);
		if (groups != null)
			return groups;
		else
			return new ArrayList<>();
	}

	@Override
	public long storeUser(String sid, WSUser user) throws WebserviceException, PersistenceException {
		return client.storeUser(sid, user);
	}

	@Override
	public long storeGroup(String sid, WSGroup group) throws WebserviceException, PersistenceException {
		return client.storeGroup(sid, group);
	}

	@Override
	public void deleteUser(String sid, long userId)
			throws PermissionException, WebserviceException, PersistenceException {
		client.deleteUser(sid, userId);
	}

	@Override
	public void deleteGroup(String sid, long groupId)
			throws PermissionException, PersistenceException, WebserviceException {
		client.deleteGroup(sid, groupId);
	}

	@Override
	public int changePassword(String sid, long userId, String oldPassword, String newPassword)
			throws WebserviceException, PersistenceException {
		return client.changePassword(sid, userId, oldPassword, newPassword);
	}

	@Override
	public WSUser getUser(String sid, long userId) throws WebserviceException, PersistenceException {
		return client.getUser(sid, userId);
	}

	@Override
	public WSUser getUserByUsername(String sid, String username) throws WebserviceException, PersistenceException {
		return client.getUserByUsername(sid, username);
	}

	@Override
	public WSGroup getGroup(String sid, long groupId) throws WebserviceException, PersistenceException {
		return client.getGroup(sid, groupId);
	}
}