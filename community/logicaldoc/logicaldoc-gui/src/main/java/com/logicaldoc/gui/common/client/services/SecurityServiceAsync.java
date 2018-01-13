package com.logicaldoc.gui.common.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIGroup;
import com.logicaldoc.gui.common.client.beans.GUIInfo;
import com.logicaldoc.gui.common.client.beans.GUIMenu;
import com.logicaldoc.gui.common.client.beans.GUISecuritySettings;
import com.logicaldoc.gui.common.client.beans.GUISequence;
import com.logicaldoc.gui.common.client.beans.GUISession;
import com.logicaldoc.gui.common.client.beans.GUIUser;

public interface SecurityServiceAsync {

	void logout(AsyncCallback<Void> callback);

	void changePassword(long userId, String oldPassword, String newPassword, boolean notify,
			AsyncCallback<Integer> callback);

	void deleteUser(long userId, AsyncCallback<Void> callback);

	void saveUser(GUIUser user, GUIInfo info, AsyncCallback<GUIUser> callback);

	void getUser(long userId, AsyncCallback<GUIUser> callback);

	void getGroup(long groupId, AsyncCallback<GUIGroup> callback);

	void deleteGroup(long groupId, AsyncCallback<Void> callback);

	void saveGroup(GUIGroup group, AsyncCallback<GUIGroup> callback);

	void removeFromGroup(long groupId, long[] docIds, AsyncCallback<Void> callback);

	void addUserToGroup(long groupId, long userId, AsyncCallback<Void> callback);

	void loadSettings(AsyncCallback<GUISecuritySettings> callback);

	void saveSettings(GUISecuritySettings settings, AsyncCallback<Boolean> callback);

	void kill(String sid, AsyncCallback<Void> callback);

	void saveProfile(GUIUser user, AsyncCallback<GUIUser> callback);

	void applyRights(GUIMenu menu, AsyncCallback<Void> callback);

	void getMenu(long id, AsyncCallback<GUIMenu> callback);

	void searchUsers(String username, String groupId, AsyncCallback<GUIUser[]> callback);

	void getSession(String locale, AsyncCallback<GUISession> callback);

	void loadBlockedEntities(AsyncCallback<GUISequence[]> callback);

	void removeBlockedEntities(long[] id, AsyncCallback<Void> callback);
}
