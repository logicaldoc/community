package com.logicaldoc.gui.common.client.services;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIGroup;
import com.logicaldoc.gui.common.client.beans.GUIInfo;
import com.logicaldoc.gui.common.client.beans.GUIMenu;
import com.logicaldoc.gui.common.client.beans.GUISecuritySettings;
import com.logicaldoc.gui.common.client.beans.GUISequence;
import com.logicaldoc.gui.common.client.beans.GUISession;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.beans.GUIValue;

public interface SecurityServiceAsync {

	void logout(AsyncCallback<Void> callback);

	void changePassword(Long requestorUserId, long userId, String oldPassword, String newPassword, boolean notify,
			AsyncCallback<GUIValue> callback);

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

	void saveAvatar(long userId, AsyncCallback<Void> callback);

	void saveACL(GUIMenu menu, AsyncCallback<Void> callback);

	void getMenu(long id, String locale, AsyncCallback<GUIMenu> callback);

	void searchUsers(String username, String groupId, AsyncCallback<GUIUser[]> callback);

	void getSession(String locale, String sid, AsyncCallback<GUISession> callback);

	void loadBlockedEntities(AsyncCallback<GUISequence[]> callback);

	void removeBlockedEntities(long[] id, AsyncCallback<Void> callback);

	void saveInterfaceSettings(GUIUser user, AsyncCallback<GUIUser> callback);

	void replicateUsersSettings(long masterUserId, Long[] userIds, boolean gui, boolean groups,
			AsyncCallback<Void> callback);

	void deleteMenu(long menuId, AsyncCallback<Void> callback);

	void getMenus(long parentId, String locale, boolean enabledOnly, AsyncCallback<GUIMenu[]> callback);

	void saveMenus(GUIMenu[] menus, String locale, AsyncCallback<Void> callback);

	void saveMenu(GUIMenu menu, String locale, AsyncCallback<GUIMenu> callback);

	void trustDevice(String label, AsyncCallback<String> callback);

	void isTrustedDevice(String device, AsyncCallback<Boolean> callback);

	void deleteTrustedDevices(String[] deviceIds, AsyncCallback<Void> callback);

	void syncGeolocationDB(String key, AsyncCallback<String> callback);

	void resetAvatar(long userId, AsyncCallback<Void> callback);

	void cloneWorkTimes(long srcUserId, List<Long> userIds, long[] groupIds, AsyncCallback<Void> callback);

	void changeStatus(long userId, boolean enabled, AsyncCallback<Void> callback);

	void updateDeviceLabel(long deviceId, String label, AsyncCallback<Void> callback);

	void generatePassword(AsyncCallback<String> callback);

	void generatePassword2(int length, int uppercaseChars, int lowercaseChars, int digits, int specialChars,
			int maxSequenceSize, int maxOccurrences, AsyncCallback<String> callback);

	void validatePassword(String password, int length, int uppercaseChars, int lowercaseChars, int digits,
			int specialChars, int maxSequenceSize, int maxOccurrences, AsyncCallback<String[]> callback);
}
