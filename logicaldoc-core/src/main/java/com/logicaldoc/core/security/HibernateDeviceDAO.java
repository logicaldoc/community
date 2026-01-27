package com.logicaldoc.core.security;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Repository;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;

import jakarta.annotation.Resource;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.transaction.Transactional;

/**
 * An Hibernate based implementation of {@link DeviceDAO}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.3
 */
@Repository("deviceDAO")
@Transactional
public class HibernateDeviceDAO extends HibernatePersistentObjectDAO<Device> implements DeviceDAO {

	private static final String USER_ID_EQUAL_USER_ID = ".userId = :userId";

	private static final String AND = " and ";

	private static final String USER_ID = "userId";

	@Resource(name = "userDAO")
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

			return findByWhere(ENTITY + ".trusted = true and " + ENTITY + USER_ID_EQUAL_USER_ID, params,
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
		else
			BeanUtils.copyProperties(requestDevice, device, "id", "recordVersion", "tenantId");

		if (StringUtils.isNotEmpty(requestDevice.getLabel()))
			device.setLabel(requestDevice.getLabel());
		device.setTenantId(user.getTenantId());
		device.setTrusted(true);
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
		return trustedDevices.stream().anyMatch(d -> d.getDeviceId().equals(requestDevice.getDeviceId()));
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