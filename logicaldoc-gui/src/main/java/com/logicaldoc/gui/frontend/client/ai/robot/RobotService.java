package com.logicaldoc.gui.frontend.client.ai.robot;

import java.util.List;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;

/**
 * The client side stub for the Robot Service. This service gives all needed
 * methods to handle robots.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
@RemoteServiceRelativePath("robot")
public interface RobotService extends RemoteService {
	/**
	 * Deletes some robots
	 * 
	 * @param robotIds identifiers of the robots
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void delete(List<Long> robotIds) throws ServerException;

	/**
	 * Creates or updates a robot
	 * 
	 * @param robot the robot to save
	 * 
	 * @return the saved sampler
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIRobot save(GUIRobot robot) throws ServerException;

	/**
	 * Retrieves a robot from the data layer
	 * 
	 * @param robotId identifier of the robot
	 * 
	 * @return the sampler
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIRobot get(long robotId) throws ServerException;

	/**
	 * Clones a robot
	 * 
	 * @param robotId Identifier of the robot to clone
	 * @param newName The name to give to the clone
	 * 
	 * @return The clone
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIRobot clone(long robotId, String newName) throws ServerException;

	/**
	 * Enables/Disabled a robot
	 * 
	 * @param robotId Identifier of the robot to change
	 * @param enable The new enabled status
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void enable(long robotId, boolean enable) throws ServerException;

	/**
	 * Asks a question to the robot
	 * 
	 * @param robotId Identifier of the robot to ask
	 * @param question The question to ask
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public String ask(long robotId, String question) throws ServerException;

	/**
	 * Saves an uploaded image as the robot's avatar
	 * 
	 * @param robotId Identifier of the robot
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public void saveAvatar(long robotId) throws ServerException;

	/**
	 * Resets the avatar to the default one
	 * 
	 * @param userId Identifier of the robot
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public void resetAvatar(long robotId) throws ServerException;
	
	public static class Instance {
		private static RobotServiceAsync inst;

		private Instance() {
		}

		public static RobotServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(RobotService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}