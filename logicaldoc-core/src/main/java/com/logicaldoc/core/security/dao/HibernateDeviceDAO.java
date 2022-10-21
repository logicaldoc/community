package com.logicaldoc.core.security.dao;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

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

	private UserDAO userDAO;

	private HibernateDeviceDAO() {
		super(Device.class);
		super.log = LoggerFactory.getLogger(HibernateDeviceDAO.class);
	}

	@Override
	public Device findByDeviceId(String deviceId) {
		try {
			Map<String, Object> params = new HashMap<String, Object>();
			params.put("deviceId", deviceId);

			List<Device> devices = findByWhere(ALIAS_ENTITY + ".deviceId = :deviceId", params, null, null);
			return devices.isEmpty() ? null : devices.get(0);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return null;
		}
	}

	@Override
	public List<Device> findTrustedDevices(long userId) {
		try {
			Map<String, Object> params = new HashMap<String, Object>();
			params.put("userId", userId);

			return findByWhere(ALIAS_ENTITY + ".trusted=1 and " + ALIAS_ENTITY + ".userId = :userId", params,
					ALIAS_ENTITY + ".lastLogin desc", null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<Device>();
		}
	}

	@Override
	public List<Device> findByUserId(long userId) {
		try {
			Map<String, Object> params = new HashMap<String, Object>();
			params.put("userId", userId);

			return findByWhere(ALIAS_ENTITY + ".userId = :userId", params, ALIAS_ENTITY + ".lastLogin desc", null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<Device>();
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
	public boolean store(Device entity) throws PersistenceException {
		if (StringUtils.isEmpty(entity.getDeviceId()))
			entity.setDeviceId(UUID.randomUUID().toString());
		return super.store(entity);
	}

	@Override
	public boolean isTrustedDevice(String username, HttpServletRequest request) {
		User user = userDAO.findByUsername(username);
		if (user == null)
			return false;

		Device requestDevice = new Device(request);
		if (requestDevice == null || requestDevice.getDeviceId() == null)
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
		else {
			try {
				Map<String, Object> params = new HashMap<String, Object>();

				StringBuilder query = new StringBuilder();

				query.append(ALIAS_ENTITY + ".userId = :userId");
				params.put("userId", device.getUserId());

				query.append(" and ");
				if (device.getBrowser() != null) {
					query.append(ALIAS_ENTITY + ".browser = :browser");
					params.put("browser", device.getBrowser());
				} else
					query.append(ALIAS_ENTITY + ".browser is null");

				query.append(" and ");
				if (device.getBrowserVersion() != null) {
					query.append(ALIAS_ENTITY + ".browserVersion = :browserVersion");
					params.put("browserVersion", device.getBrowserVersion());
				} else
					query.append(ALIAS_ENTITY + ".browserVersion is null");

				query.append(" and ");
				if (device.getOperativeSystem() != null) {
					query.append(ALIAS_ENTITY + ".operativeSystem = :operativeSystem");
					params.put("operativeSystem", device.getOperativeSystem());
				} else
					query.append(ALIAS_ENTITY + ".operativeSystem is null");

				query.append(" and ");
				if (device.getType() != null) {
					query.append(ALIAS_ENTITY + ".type = :type");
					params.put("type", device.getType());
				} else
					query.append(ALIAS_ENTITY + ".type is null");

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
	}

	@Override
	public boolean delete(long deviceId, int code) {
		if (!checkStoringAspect())
			return false;

		boolean result = true;

		try {
			Device device = (Device) findById(deviceId);
			if (device != null) {
				device.setDeleted(code);
				device.setDeviceId(device.getId() + "." + device.getDeviceId());
				saveOrUpdate(device);
			}
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			result = false;
		}

		return result;
	}

	public void setUserDAO(UserDAO userDAO) {
		this.userDAO = userDAO;
	}
}