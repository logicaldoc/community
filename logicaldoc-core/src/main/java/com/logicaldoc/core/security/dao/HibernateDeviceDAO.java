package com.logicaldoc.core.security.dao;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
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
			List<Device> devices = findByWhere("_entity.deviceId = ?1", new Object[] { deviceId }, null, null);
			return devices.isEmpty() ? null : devices.get(0);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return null;
		}
	}

	@Override
	public List<Device> findTrustedDevices(long userId) {
		try {
			return findByWhere("_entity.trusted=1 and _entity.userId = ?1", new Object[] { userId },
					"_entity.lastLogin desc", null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<Device>();
		}
	}

	@Override
	public List<Device> findByUserId(long userId) {
		try {
			return findByWhere("_entity.userId = ?1", new Object[] { userId }, "_entity.lastLogin desc", null);
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
		if (ttl > 0) {
			Date today = new Date();
			GregorianCalendar cal = new GregorianCalendar();
			cal.add(Calendar.DAY_OF_MONTH, -ttl);
			Date ldDate = cal.getTime();

			log.debug("today: {}", today);
			log.debug("ldDate: {}", ldDate);

			try {
				int rowsUpdated = jdbcUpdate("UPDATE ld_device SET ld_deleted = 1, ld_lastmodified = ?"
						+ " WHERE ld_deleted = 0 AND ld_lastlogin < ?", today, ldDate);
				log.info("cleanOldDevices rows updated: {}", rowsUpdated);
			} catch (Exception e) {
				log.error(e.getMessage(), e);
			}

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
				List<Object> params = new ArrayList<Object>();
				StringBuffer query = new StringBuffer();
				int i = 1;

				query.append("_entity.userId = ?" + (i++));
				params.add(device.getUserId());

				query.append(" and ");
				if (device.getBrowser() != null) {
					query.append("_entity.browser = ?" + (i++));
					params.add(device.getBrowser());
				} else
					query.append("_entity.browser is null");

				query.append(" and ");
				if (device.getBrowserVersion() != null) {
					query.append("_entity.browserVersion = ?" + (i++));
					params.add(device.getBrowserVersion());
				} else
					query.append("_entity.browserVersion is null");

				query.append(" and ");
				if (device.getOperativeSystem() != null) {
					query.append("_entity.operativeSystem = ?" + (i++));
					params.add(device.getOperativeSystem());
				} else
					query.append("_entity.operativeSystem is null");

				query.append(" and ");
				if (device.getType() != null) {
					query.append("_entity.type = ?" + (i++));
					params.add(device.getType());
				} else
					query.append("_entity.type is null");

				List<Device> devices = findByWhere(query.toString(), params.toArray(new Object[0]), null, null);
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