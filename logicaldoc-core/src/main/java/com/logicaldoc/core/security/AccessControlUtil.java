package com.logicaldoc.core.security;

import java.util.Iterator;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.user.Group;
import com.logicaldoc.core.security.user.GroupDAO;
import com.logicaldoc.util.Context;

/**
 * Some utility methods for the access control
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.1
 *
 */
public class AccessControlUtil {

	private AccessControlUtil() {
	}

	public static void removeForbiddenPermissionsForGuests(Securable securable) throws PersistenceException {
		// Remove the forbidden permissions for the guests
		GroupDAO gDao = (GroupDAO) Context.get().getBean(GroupDAO.class);
		Iterator<AccessControlEntry> iter = securable.getAccessControlList().iterator();
		while (iter.hasNext()) {
			AccessControlEntry ace = iter.next();
			Group group = gDao.findById(ace.getGroupId());
			if (group != null && group.isGuest()) {
				ace.setAdd(0);
				ace.setArchive(0);
				ace.setAutomation(0);
				ace.setCalendar(0);
				ace.setDelete(0);
				ace.setExport(0);
				ace.setImmutable(0);
				ace.setImport(0);
				ace.setMove(0);
				ace.setPassword(0);
				ace.setRename(0);
				ace.setSecurity(0);
				ace.setSign(0);
				ace.setWorkflow(0);
				ace.setWrite(0);
				ace.setReadingreq(0);
			}
		}
	}
}