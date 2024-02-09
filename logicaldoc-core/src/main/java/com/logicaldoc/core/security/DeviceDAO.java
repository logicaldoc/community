package com.logicaldoc.core.security;

import java.util.List;

import javax.servlet.http.HttpServletRequest;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.core.security.user.User;

/**
 * A DAO to handle the devices
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.3
 */
public interface DeviceDAO extends PersistentObjectDAO<Device> {

	/**
	 * Gets the device by it's alternate key: {@link Device#getDeviceId()}
	 * 
	 * @param deviceId identifier of the device
	 * 
	 * @return the found device
	 */
	public Device findByDeviceId(String deviceId);

	/**
	 * Finds the device that corresponds to the given one
	 * 
	 * @param device the device to take as model
	 * 
	 * @return the found device
	 */
	public Device findByDevice(Device device);

	/**
	 * Finds the devices of a specific user
	 * 
	 * @param userId identifier of the user
	 * 
	 * @return the list of devices ordered by descending last login date
	 */
	public List<Device> findByUserId(long userId);

	/**
	 * Retrieves all the devices trusted by a given user
	 * 
	 * @param userId the user to consider
	 * 
	 * @return the list of devices ordered by descending last login date
	 */
	public List<Device> findTrustedDevices(long userId);

	/**
	 * Checks if the current request's device is trusted by the user
	 * 
	 * @param username username of the current user
	 * @param request the current request
	 * 
	 * @return true only if the request's device is trusted by the user
	 * 
	 * @throws PersistenceException Error in the data layer
	 */
	public boolean isTrustedDevice(String username, HttpServletRequest request) throws PersistenceException;

	/**
	 * Trusts a device for a user
	 * 
	 * @param user the current user
	 * @param device the device to trust
	 * 
	 * @return the trusted device
	 * 
	 * @throws PersistenceException an error happened in the database
	 */
	public Device trustDevice(User user, Device device) throws PersistenceException;

	/**
	 * This method deletes all the devices lastly used before the given days
	 * since now. If <code>ttl</code> is 0 or -1, the cancellation is not made.
	 * 
	 * @param ttl The maximum number of days over which the device is considered
	 *        old
	 */
	public void cleanOldDevices(int ttl);
}