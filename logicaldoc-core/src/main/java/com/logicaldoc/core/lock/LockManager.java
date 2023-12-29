package com.logicaldoc.core.lock;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;

import javax.annotation.Resource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.generic.Generic;
import com.logicaldoc.core.generic.GenericDAO;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.util.config.ContextProperties;

/**
 * Central class to manage locks
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5
 */
public class LockManager {
	private static final String LOCK = "lock";

	protected Logger log = LoggerFactory.getLogger(LockManager.class);

	@Resource(name="GenericDAO")
	private GenericDAO genericDao;

	@Resource(name="ContextProperties")
	private ContextProperties config;

	/**
	 * Gets all the transaction ids associated to the locks
	 * 
	 * @return the lists of transactions
	 */
	@SuppressWarnings("unchecked")
	public List<String> getAllTransactions() {
		try {
			return genericDao.queryForList(
					"select ld_string1 from ld_generic where ld_type='lock' and ld_string1 is not null", String.class);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	/**
	 * Acquire a lock of a given name and for a given transaction.
	 * 
	 * @param lockName Name of the lock
	 * @param transactionId Id of the transaction
	 * 
	 * @return true only if the lock was acquired
	 */
	public boolean get(String lockName, String transactionId) {
		GregorianCalendar cal = new GregorianCalendar();
		cal.add(Calendar.SECOND, config.getInt("lock.wait"));
		Date ldDate = cal.getTime();
		while (new Date().before(ldDate)) {
			if (getInternal(lockName, transactionId)) {
				log.debug("Acquired lock {}", lockName);
				return true;
			} else
				synchronized (this) {
					try {
						wait(1000);
					} catch (InterruptedException e) {
						Thread.currentThread().interrupt();
					}
				}
		}

		log.warn("Unable to get lock  {}", lockName);
		return false;
	}

	private String getSubType(String lockName) {
		return lockName + "-" + config.getProperty("id");
	}

	/**
	 * Releases a previously acquired lock.
	 * 
	 * @param lockName The lock name
	 * @param transactionId The transaction ID
	 */
	public void release(String lockName, String transactionId) {
		if (lockName == null || transactionId == null)
			return;

		Generic lock = genericDao.findByAlternateKey(LOCK, getSubType(lockName), null, Tenant.DEFAULT_ID);
		if (lock != null && transactionId.equals(lock.getString1())) {
			lock.setDate1(null);
			lock.setString1(null);

			try {
				genericDao.store(lock);
			} catch (PersistenceException e) {
				log.warn(e.getMessage(), e);
			}
		}
	}

	protected boolean getInternal(String lockName, String transactionId) {
		Date today = new Date();
		Generic lock = genericDao.findByAlternateKey(LOCK, getSubType(lockName), null, Tenant.DEFAULT_ID);
		try {
			if (lock == null) {
				log.debug("Lock {} not found", lockName);
				lock = new Generic(LOCK, getSubType(lockName));
				lock.setString1(transactionId);
				lock.setDate1(today);
			}

			GregorianCalendar cal = new GregorianCalendar();
			cal.add(Calendar.SECOND, -config.getInt("lock.ttl"));
			Date ldDate = cal.getTime();

			if (lock.getDate1() == null || lock.getDate1().before(ldDate)) {
				log.debug("Lock {} expired", lockName);
				lock.setDate1(today);
				lock.setString1(transactionId);
			}

			if (lock.getString1() == null || transactionId.equals(lock.getString1())) {
				lock.setDate1(today);
				lock.setString1(transactionId);
				return true;
			} else
				return false;
		} finally {
			try {
				genericDao.store(lock);
			} catch (PersistenceException e) {
				log.warn(e.getMessage(), e);
			}
		}
	}

	public void setGenericDao(GenericDAO genericDao) {
		this.genericDao = genericDao;
	}

	public void setConfig(ContextProperties config) {
		this.config = config;
	}
}