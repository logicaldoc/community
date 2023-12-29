package com.logicaldoc.core.security.dao;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Device;
import com.logicaldoc.core.security.User;

/**
 * An Hibernate based implementation of {@link DeviceDAO}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.3
 */
public class HibernateDeviceDAO extends HibernatePersistentObjectDAO<Device> implements DeviceDAO {

	private static final String USER_ID_EQUAL_USER_ID = ".userId = :userId";

	private static final String AND = " and ";

	private static final String USER_ID = "userId";

	@Resource(name = "UserDAO")
	private UserDAO userDAO;

	private HibernateDeviceDAO() {
		super(Device.class);
		super.log = LoggerFactory.getLogger(HibernateDeviceDAO.class);
	}

	@Override
	public Device findByDeviceId(String deviceId) {
		try {
			Map<String, Object> params = new HashMap<>();
			params.put("deviceId", deviceId);

			List<Device> devices = findByWhere(ENTITY + ".deviceId = :deviceId", params, null, null);
			return devices.isEmpty() ? null : devices.get(0);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return null;
		}
	}

	@Override
	public List<Device> findTrustedDevices(long userId) {
		try {
			Map<String, Object> params = new HashMap<>();
			params.put(USER_ID, userId);

			return findByWhere(ENTITY + ".trusted=1 and " + ENTITY + USER_ID_EQUAL_USER_ID, params,
					ENTITY + ".lastLogin desc", null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	@Override
	public List<Device> findByUserId(long userId) {
		try {
			Map<String, Object> params = new HashMap<>();
			params.put(USER_ID, userId);

			return findByWhere(ENTITY + USER_ID_EQUAL_USER_ID, params, ENTITY + ".lastLogin desc", null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	@Override
	public Device trustDevice(User user, Device requestDevice) throws PersistenceException {
		Device device = findByDevice(requestDevice);
		if (device == null)
			device = requestDevice;

		if (StringUtils.isNotEmpty(requestDevice.getLabel()))
			device.setLabel(requestDevice.getLabel());
		device.setTrusted(1);
		device.setUserId(user.getId());
		device.setUsername(user.getFullName());
		store(device);

		return device;
	}

	@Override
	public void cleanOldDevices(int ttl) {
		try {
			log.info("cleanOldDevices rows updated: {}", cleanOldRecords(ttl, "ld_device", "ld_creation"));
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
	}

	@Override
	public void store(Device entity) throws PersistenceException {
		if (StringUtils.isEmpty(entity.getDeviceId()))
			entity.setDeviceId(UUID.randomUUID().toString());
		super.store(entity);
	}

	@Override
	public boolean isTrustedDevice(String username, HttpServletRequest request) throws PersistenceException {
		User user = userDAO.findByUsername(username);
		if (user == null)
			return false;

		Device requestDevice = new Device(request);
		if (requestDevice.getDeviceId() == null)
			return false;

		List<Device> trustedDevices = findTrustedDevices(user.getId());
		for (Device device : trustedDevices)
			if (device.equals(requestDevice))
				return true;

		return false;
	}

	@Override
	public Device findByDevice(Device device) {
		if (device.getDeviceId() != null)
			return findByDeviceId(device.getDeviceId());

		Map<String, Object> params = new HashMap<>();

		StringBuilder query = new StringBuilder();

		query.append(ENTITY + USER_ID_EQUAL_USER_ID);
		params.put(USER_ID, device.getUserId());

		query.append(AND);
		if (device.getBrowser() != null) {
			query.append(ENTITY + ".browser = :browser");
			params.put("browser", device.getBrowser());
		} else
			query.append(ENTITY + ".browser is null");

		query.append(AND);
		if (device.getBrowserVersion() != null) {
			query.append(ENTITY + ".browserVersion = :browserVersion");
			params.put("browserVersion", device.getBrowserVersion());
		} else
			query.append(ENTITY + ".browserVersion is null");

		query.append(AND);
		if (device.getOperativeSystem() != null) {
			query.append(ENTITY + ".operativeSystem = :operativeSystem");
			params.put("operativeSystem", device.getOperativeSystem());
		} else
			query.append(ENTITY + ".operativeSystem is null");

		query.append(AND);
		if (device.getType() != null) {
			query.append(ENTITY + ".type = :type");
			params.put("type", device.getType());
		} else
			query.append(ENTITY + ".type is null");

		try {
			List<Device> devices = findByWhere(query.toString(), params, null, null);
			if (devices.isEmpty())
				return null;
			else
				return devices.get(0);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return null;
		}
	}

	@Override
	public void delete(long deviceId, int code) throws PersistenceException {
		if (!checkStoringAspect())
			return;

		Device device = findById(deviceId);
		if (device != null) {
			device.setDeleted(code);
			device.setDeviceId(device.getId() + "." + device.getDeviceId());
			saveOrUpdate(device);
		}
	}

	public void setUserDAO(UserDAO userDAO) {
		this.userDAO = userDAO;
	}
}