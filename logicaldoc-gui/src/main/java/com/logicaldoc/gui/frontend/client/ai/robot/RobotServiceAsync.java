package com.logicaldoc.gui.frontend.client.ai.robot;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;

public interface RobotServiceAsync {

	void delete(List<Long> robotIds, AsyncCallback<Void> callback);

	void save(GUIRobot robot, AsyncCallback<GUIRobot> callback);

	void get(long robotId, AsyncCallback<GUIRobot> callback);

	void clone(long robotId, String newName, AsyncCallback<GUIRobot> callback);

	void enable(long robotId, boolean enable, AsyncCallback<Void> callback);

	void ask(long robotId, String question, AsyncCallback<String> callback);

	void saveAvatar(long robotId, AsyncCallback<Void> callback);

	void resetAvatar(long robotId, AsyncCallback<Void> callback);
}