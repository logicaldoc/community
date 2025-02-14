package com.logicaldoc.web.service;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.dashlet.Dashlet;
import com.logicaldoc.core.dashlet.DashletDAO;
import com.logicaldoc.core.generic.Generic;
import com.logicaldoc.core.generic.GenericDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.menu.Menu;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIDashlet;
import com.logicaldoc.gui.frontend.client.services.DashletService;
import com.logicaldoc.util.Context;

/**
 * The dashlet service for the operations on the dashlets done through the GUI.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.3
 */
public class DashletServiceImpl extends AbstractRemoteService implements DashletService {

	private static final long serialVersionUID = 1L;

	private static final Logger log = LoggerFactory.getLogger(DashletServiceImpl.class);

	@Override
	public void save(GUIDashlet guiDashlet) throws ServerException {
		Session session = validateSession();
		checkMenu(getThreadLocalRequest(), Menu.SETTINGS);
		try {
			DashletDAO dao = Context.get(DashletDAO.class);
			Dashlet dashlet = dao.findByName(guiDashlet.getName(), session.getTenantId());
			if (dashlet == null) {
				dashlet = toDashlet(guiDashlet);
				dashlet.setId(0L);
			}
			dashlet.setContent(guiDashlet.getContent());
			dashlet.setMax(guiDashlet.getMax());
			dashlet.setColumns(guiDashlet.getColumns());
			dashlet.setUnique(guiDashlet.isUnique() ? 1 : 0);
			dashlet.setQuery(guiDashlet.getQuery());
			dashlet.setTitle(guiDashlet.getTitle());
			dashlet.setType(guiDashlet.getType());

			dao.store(dashlet);
		} catch (Exception e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public void saveDashlets(List<GUIDashlet> dashlets) throws ServerException {
		for (GUIDashlet guiDashlet : dashlets)
			save(guiDashlet);
	}

	@Override
	public List<GUIDashlet> loadDashlets() throws ServerException {
		Session session = validateSession();
		try {
			DashletDAO dao = Context.get(DashletDAO.class);
			List<Dashlet> dashlets = dao.findAll(session.getTenantId());
			ArrayList<GUIDashlet> guiDashlets = new ArrayList<>();
			for (Dashlet dashlet : dashlets)
				guiDashlets.add(fromDashlet(dashlet));
			return guiDashlets;
		} catch (Exception e) {
			return throwServerException(session, log, e);
		}
	}

	@Override
	public GUIDashlet get(long dashletId) throws ServerException {
		Session session = validateSession();
		try {
			DashletDAO dao = Context.get(DashletDAO.class);
			Dashlet dashlet = dao.findById(dashletId);
			if (dashlet == null)
				throw new ServerException("Unexisting dashlet " + dashletId);
			return fromDashlet(dashlet);
		} catch (Exception e) {
			return throwServerException(session, log, e);
		}
	}

	@Override
	public GUIDashlet get(String name) throws ServerException {
		Session session = validateSession();
		try {
			DashletDAO dao = Context.get(DashletDAO.class);
			Dashlet dashlet = dao.findByName(name, session.getTenantId());
			if (dashlet == null)
				throw new ServerException("Unexisting dashlet " + name);
			return fromDashlet(dashlet);
		} catch (Exception e) {
			return throwServerException(session, log, e);
		}
	}

	@Override
	public void delete(long dashletId) throws ServerException {
		Session session = validateSession();
		checkMenu(getThreadLocalRequest(), Menu.SETTINGS);
		try {
			DashletDAO dao = Context.get(DashletDAO.class);
			dao.delete(dashletId);
		} catch (Exception e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public void saveUserDashlets(List<GUIDashlet> dashlets) throws ServerException {
		Session session = validateSession();
		GenericDAO gDao = Context.get(GenericDAO.class);
		UserDAO uDao = Context.get(UserDAO.class);

		try {
			/*
			 * Delete the actual dashlets for this user
			 */
			Map<String, Generic> settings = uDao.findUserSettings(session.getUserId(), "dashlet");
			for (Generic setting : settings.values())
				gDao.delete(setting.getId());

			/*
			 * Now save the new dashlets
			 */
			for (GUIDashlet dashlet : dashlets) {
				Generic generic = new Generic("usersetting", "dashlet-" + dashlet.getName(), session.getUserId());
				generic.setInteger1(dashlet.getId());
				generic.setInteger2((long) dashlet.getColumn());
				generic.setInteger3((long) dashlet.getRow());
				generic.setString1(Long.toString(dashlet.getIndex()));
				gDao.store(generic);
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}
	}

	private Dashlet toDashlet(GUIDashlet guiDashlet) {
		Dashlet dashlet = new Dashlet();
		dashlet.setId(guiDashlet.getId());
		dashlet.setName(guiDashlet.getName());
		dashlet.setType(guiDashlet.getType());
		dashlet.setTitle(guiDashlet.getTitle());
		dashlet.setQuery(guiDashlet.getQuery());
		dashlet.setContent(guiDashlet.getContent());
		dashlet.setMax(guiDashlet.getMax());
		dashlet.setColumns(guiDashlet.getColumns());
		dashlet.setUnique(guiDashlet.isUnique() ? 1 : 0);
		return dashlet;
	}

	private GUIDashlet fromDashlet(Dashlet dashlet) {
		GUIDashlet guiDashlet = new GUIDashlet();
		guiDashlet.setId(dashlet.getId());
		guiDashlet.setName(dashlet.getName());
		guiDashlet.setType(dashlet.getType());
		guiDashlet.setTitle(dashlet.getTitle());
		guiDashlet.setQuery(dashlet.getQuery());
		guiDashlet.setContent(dashlet.getContent());
		guiDashlet.setMax(dashlet.getMax());
		guiDashlet.setColumns(dashlet.getColumns());
		guiDashlet.setUnique(dashlet.getUnique() == 1);
		return guiDashlet;
	}
}