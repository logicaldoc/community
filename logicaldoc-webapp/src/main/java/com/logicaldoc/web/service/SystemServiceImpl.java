package com.logicaldoc.web.service;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.java.plugin.registry.PluginDescriptor;
import org.quartz.SchedulerException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.RowMapper;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.RunLevel;
import com.logicaldoc.core.dbinit.PluginDbInit;
import com.logicaldoc.core.document.DocumentHistoryDAO;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.generic.Generic;
import com.logicaldoc.core.generic.GenericDAO;
import com.logicaldoc.core.history.History;
import com.logicaldoc.core.job.JobManager;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.TenantDAO;
import com.logicaldoc.core.security.menu.Menu;
import com.logicaldoc.core.security.user.Group;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.core.stats.StatsCollector;
import com.logicaldoc.core.task.Task;
import com.logicaldoc.core.task.TaskManager;
import com.logicaldoc.core.task.TaskScheduling;
import com.logicaldoc.core.task.TaskTrigger;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.gui.common.client.AccessDeniedException;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIHistory;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUIScheduling;
import com.logicaldoc.gui.common.client.beans.GUITask;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.logicaldoc.gui.frontend.client.services.SystemService;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.config.LogConfigurator;
import com.logicaldoc.util.config.PluginDescriptorConfigurator;
import com.logicaldoc.util.exec.Exec;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.io.ZipUtil;
import com.logicaldoc.util.plugin.LogicalDOCPlugin;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.plugin.PluginRegistry;
import com.logicaldoc.util.sql.SqlUtil;
import com.logicaldoc.web.UploadServlet;
import com.logicaldoc.web.data.LogDataServlet;
import com.logicaldoc.web.listener.ApplicationListener;
import com.logicaldoc.web.websockets.WebsocketTool;

/**
 * Implementation of the SystemService
 * 
 * @author Matteo Caruso - LogicalDOC
 * 
 * @since 6.0
 */
public class SystemServiceImpl extends AbstractRemoteService implements SystemService {

	private static final String AND = " and ";

	private static final String SECONDS = "seconds";

	private static final String TRASH = "trash";

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(SystemServiceImpl.class);

	protected static File defaultWebappRootFolder = null;

	@Override
	public boolean disableTask(String taskName) throws ServerException {
		Session session = validateSession();

		Task task = getTask(taskName);

		if (task == null)
			throw new ServerException("Cannot find task named " + taskName);

		task.getScheduling().setEnabled(false);
		try {
			task.getScheduling().save();
			return true;
		} catch (IOException | ParseException e) {
			return throwServerException(session, log, e);
		}
	}

	@Override
	public boolean enableTask(String taskName) throws ServerException {
		Session session = validateSession();

		Task task = getTask(taskName);
		if (task == null)
			throw new ServerException("Cannot find task named " + taskName);

		try {
			TaskScheduling scheduling = task.getScheduling();
			if (scheduling != null) {
				task.getScheduling().setEnabled(true);
				task.getScheduling().save();
			} else {
				log.warn("No scheduling informations found for task {}", taskName);
			}

			return true;
		} catch (IOException | ParseException e) {
			return throwServerException(session, log, e);
		}
	}

	@Override
	public List<List<GUIParameter>> getStatistics(String locale) throws ServerException {
		Session session = validateSession();

		try {
			GenericDAO genDao = Context.get().getBean(GenericDAO.class);

			List<List<GUIParameter>> parameters = new ArrayList<>();

			/*
			 * Repository statistics
			 */
			Generic gen = genDao.findByAlternateKey(StatsCollector.STAT, TRASH, null, session.getTenantId());
			GUIParameter trashSize = new GUIParameter();
			trashSize.setName(TRASH);
			setLongValue(gen, trashSize);

			gen = genDao.findByAlternateKey(StatsCollector.STAT, "docdir", null, session.getTenantId());
			GUIParameter docDirSize = new GUIParameter();
			docDirSize.setName("documents");
			setLongValue(gen, docDirSize);
			docDirSize.setValue(
					Long.toString(Long.parseLong(docDirSize.getValue()) - Long.parseLong(trashSize.getValue())));

			gen = genDao.findByAlternateKey(StatsCollector.STAT, "userdir", null, Tenant.SYSTEM_ID);
			GUIParameter userDirSize = new GUIParameter();
			userDirSize.setName("users");
			setLongValue(gen, userDirSize);

			gen = genDao.findByAlternateKey(StatsCollector.STAT, "indexdir", null, Tenant.SYSTEM_ID);
			GUIParameter indexDirSize = new GUIParameter();
			indexDirSize.setName("fulltextindex");
			setLongValue(gen, indexDirSize);

			gen = genDao.findByAlternateKey(StatsCollector.STAT, "importdir", null, Tenant.SYSTEM_ID);
			GUIParameter importDirSize = new GUIParameter();
			importDirSize.setName("iimport");
			setLongValue(gen, importDirSize);

			gen = genDao.findByAlternateKey(StatsCollector.STAT, "exportdir", null, Tenant.SYSTEM_ID);
			GUIParameter exportDirSize = new GUIParameter();
			exportDirSize.setName("eexport");
			setLongValue(gen, exportDirSize);

			gen = genDao.findByAlternateKey(StatsCollector.STAT, "plugindir", null, Tenant.SYSTEM_ID);
			GUIParameter pluginsDirSize = new GUIParameter();
			pluginsDirSize.setName("plugins");
			setLongValue(gen, pluginsDirSize);

			gen = genDao.findByAlternateKey(StatsCollector.STAT, "dbdir", null, Tenant.SYSTEM_ID);
			GUIParameter dbDirSize = new GUIParameter();
			dbDirSize.setName("database");
			setLongValue(gen, dbDirSize);

			gen = genDao.findByAlternateKey(StatsCollector.STAT, "logdir", null, Tenant.SYSTEM_ID);
			GUIParameter logsDirSize = new GUIParameter();
			logsDirSize.setName("logs");
			setLongValue(gen, logsDirSize);

			parameters.add(Arrays.asList(docDirSize, trashSize, userDirSize, indexDirSize, importDirSize, exportDirSize,
					pluginsDirSize, dbDirSize, logsDirSize));

			/*
			 * Documents statistics
			 */
			gen = genDao.findByAlternateKey(StatsCollector.STAT, "notindexeddocs", null, session.getTenantId());
			GUIParameter notIndexed = new GUIParameter();
			notIndexed.setName("notindexed");
			setLongValue(gen, notIndexed);

			gen = genDao.findByAlternateKey(StatsCollector.STAT, "notindexabledocs", null, session.getTenantId());
			GUIParameter notIndexableDocs = new GUIParameter();
			notIndexableDocs.setName("notindexabledocs");
			notIndexableDocs.setLabel("notindexable");
			setLongValue(gen, notIndexableDocs);

			gen = genDao.findByAlternateKey(StatsCollector.STAT, "indexeddocs", null, session.getTenantId());
			GUIParameter indexed = new GUIParameter();
			indexed.setName("indexed");
			setLongValue(gen, indexed);

			gen = genDao.findByAlternateKey(StatsCollector.STAT, "deleteddocs", null, session.getTenantId());
			GUIParameter deletedDocs = new GUIParameter();
			deletedDocs.setName("docstrash");
			deletedDocs.setLabel(TRASH);
			setLongValue(gen, deletedDocs);

			gen = genDao.findByAlternateKey(StatsCollector.STAT, "archiveddocs", null, session.getTenantId());
			GUIParameter archivedDocs = new GUIParameter();
			archivedDocs.setName("archiveddocs");
			archivedDocs.setLabel("archiveds");
			setLongValue(gen, archivedDocs);

			parameters.add(Arrays.asList(notIndexed, notIndexableDocs, indexed, deletedDocs, archivedDocs));

			/*
			 * Pages statistics
			 */
			gen = genDao.findByAlternateKey(StatsCollector.STAT, "notindexedpages", null, session.getTenantId());
			GUIParameter notIndexedPages = new GUIParameter();
			notIndexedPages.setName("notindexed");
			setLongValue(gen, notIndexedPages);

			gen = genDao.findByAlternateKey(StatsCollector.STAT, "notindexablepages", null, session.getTenantId());
			GUIParameter notIndexablePages = new GUIParameter();
			notIndexablePages.setName("notindexablepages");
			notIndexablePages.setLabel("notindexable");
			setLongValue(gen, notIndexablePages);

			gen = genDao.findByAlternateKey(StatsCollector.STAT, "indexedpages", null, session.getTenantId());
			GUIParameter indexedPages = new GUIParameter();
			indexedPages.setName("indexed");
			setLongValue(gen, indexedPages);

			gen = genDao.findByAlternateKey(StatsCollector.STAT, "deletedpages", null, session.getTenantId());
			GUIParameter deletedPages = new GUIParameter();
			deletedPages.setName("pagestrash");
			deletedPages.setLabel(TRASH);
			setLongValue(gen, deletedPages);

			gen = genDao.findByAlternateKey(StatsCollector.STAT, "archivedpages", null, session.getTenantId());
			GUIParameter archivedPages = new GUIParameter();
			archivedPages.setName("archivedpages");
			archivedPages.setLabel("archiveds");
			setLongValue(gen, archivedPages);

			parameters.add(Arrays.asList(notIndexedPages, notIndexableDocs, indexedPages, deletedPages, archivedPages));

			/*
			 * Folders statistics
			 */
			gen = genDao.findByAlternateKey(StatsCollector.STAT, "withdocs", null, session.getTenantId());
			GUIParameter notEmptyFolders = new GUIParameter();
			notEmptyFolders.setName("withdocs");
			setLongValue(gen, notEmptyFolders);

			gen = genDao.findByAlternateKey(StatsCollector.STAT, "empty", null, session.getTenantId());
			GUIParameter emptyFolders = new GUIParameter();
			emptyFolders.setName("empty");
			setLongValue(gen, emptyFolders);

			gen = genDao.findByAlternateKey(StatsCollector.STAT, "deletedfolders", null, session.getTenantId());
			GUIParameter deletedFolders = new GUIParameter();
			deletedFolders.setName("folderstrash");
			deletedFolders.setLabel(TRASH);
			setLongValue(gen, deletedFolders);

			parameters.add(Arrays.asList(notEmptyFolders, emptyFolders, deletedFolders));

			/*
			 * Last run
			 */
			gen = genDao.findByAlternateKey(StatsCollector.STAT, "lastrun", null, Tenant.SYSTEM_ID);
			Date date = gen != null ? gen.getDate1() : null;
			GUIParameter lastrun = new GUIParameter();
			lastrun.setName("lastrun");
			if (date != null) {
				DateFormat df2 = new SimpleDateFormat(I18N.message("format_date", locale));
				lastrun.setValue(df2.format(date));
			} else {
				lastrun.setValue("");
			}

			parameters.add(Arrays.asList(lastrun));

			return parameters;
		} catch (PersistenceException e) {
			return throwServerException(session, log, e);
		}
	}

	private void setLongValue(Generic generic, GUIParameter parameter) {
		if (generic != null)
			parameter.setValue(Long.toString(generic.getInteger1()));
		else
			parameter.setValue("0");
	}

	@Override
	public GUITask getTaskByName(String taskName, String locale) throws ServerException {
		validateSession();

		Task tsk = getTask(taskName);
		if (tsk == null)
			return null;

		GUITask task = new GUITask();
		task.setName(tsk.getName());
		task.setStatus(tsk.getStatus());
		task.setProgress((int) tsk.getProgress());
		task.setSize(tsk.getSize());
		task.setCompletionPercentage(tsk.getCompletionPercentage());
		task.setIndeterminate(tsk.isIndeterminate());
		task.setSendActivityReport(tsk.isSendActivityReport());

		setScheduling(task, tsk, locale);

		/*
		 * Parse the recipients ids and check the users existence
		 */
		addReportRecipients(task, tsk);

		return task;
	}

	private Task getTask(String taskName) {
		TaskManager manager = Context.get().getBean(TaskManager.class);
		Task tsk = null;
		for (Task t : manager.getTasks()) {
			if (t.getName().equals(taskName)) {
				tsk = t;
				break;
			}
		}
		return tsk;
	}

	private void setScheduling(GUITask guiTask, Task task, String locale) {
		GUIScheduling scheduling = new GUIScheduling(task.getName());
		scheduling.setEnabled(task.getScheduling().isEnabled());
		scheduling.setMode(task.getScheduling().getMode());
		if (task.getScheduling().getMode().equals(TaskTrigger.MODE_SIMPLE)) {
			scheduling.setSimple(true);
			scheduling.setDelay(task.getScheduling().getDelaySeconds());
			scheduling.setInterval(task.getScheduling().getIntervalSeconds());
			guiTask.setSchedulingLabel(I18N.message("each", locale) + " " + task.getScheduling().getIntervalSeconds()
					+ " " + I18N.message(SECONDS, locale).toLowerCase());
		} else {
			scheduling.setSimple(false);
			scheduling.setCronExpression(task.getScheduling().getCronExpression());
		}

		scheduling.setMaxLength(task.getScheduling().getMaxLength());
		guiTask.setScheduling(scheduling);
	}

	private void addReportRecipients(GUITask guiTask, Task task) {
		UserDAO dao = Context.get().getBean(UserDAO.class);
		if (StringUtils.isNotEmpty(task.getReportRecipients())) {
			StringTokenizer st = new StringTokenizer(task.getReportRecipients(), ",", false);
			while (st.hasMoreTokens()) {
				try {
					User user = dao.findById(Long.parseLong(st.nextToken()));
					if (user != null) {
						GUIUser u = new GUIUser();
						u.setId(user.getId());
						u.setUsername(user.getUsername());
						u.setName(user.getName());
						u.setFirstName(user.getFirstName());
						guiTask.addReportRecipient(u);
					}
				} catch (NumberFormatException | PersistenceException e) {
					log.warn(e.getMessage(), e);
				}
			}
		}
	}

	@Override
	public List<GUITask> loadTasks(String locale) throws ServerException {
		validateSession();

		TaskManager manager = Context.get().getBean(TaskManager.class);
		List<GUITask> tasks = new ArrayList<>();

		for (Task t : manager.getTasks()) {
			GUITask task = new GUITask();
			task.setName(t.getName());
			task.setStatus(t.getStatus());
			task.setProgress((int) t.getProgress());
			task.setSize(t.getSize());
			task.setIndeterminate(t.isIndeterminate());
			task.setCompletionPercentage(t.getCompletionPercentage());

			GUIScheduling scheduling = new GUIScheduling(t.getName());
			scheduling.setEnabled(t.getScheduling().isEnabled());
			scheduling.setMode(t.getScheduling().getMode());
			if (t.getScheduling().getMode().equals(TaskTrigger.MODE_SIMPLE)) {
				scheduling.setSimple(true);
				scheduling.setDelay(t.getScheduling().getDelay());
				scheduling.setInterval(t.getScheduling().getIntervalSeconds());
				task.setSchedulingLabel(I18N.message("each", locale) + " " + t.getScheduling().getIntervalSeconds()
						+ " " + I18N.message(SECONDS, locale).toLowerCase());
			} else if (t.getScheduling().getMode().equals(TaskTrigger.MODE_CRON)) {
				scheduling.setSimple(false);
				scheduling.setCronExpression(t.getScheduling().getCronExpression());
				task.setSchedulingLabel(t.getScheduling().getCronExpression());
			}

			scheduling.setMaxLength(t.getScheduling().getMaxLength());
			scheduling.setPreviousFireTime(t.getScheduling().getPreviousFireTime());
			scheduling.setNextFireTime(t.getScheduling().getNextFireTime());

			task.setScheduling(scheduling);

			task.setSendActivityReport(t.isSendActivityReport());

			tasks.add(task);
		}

		return tasks;

	}

	@Override
	public GUITask saveTask(GUITask guiTask, String locale) throws ServerException {
		validateSession();

		TaskManager manager = Context.get().getBean(TaskManager.class);
		Task task = null;
		for (Task t : manager.getTasks()) {
			if (t.getName().equals(guiTask.getName())) {
				task = t;
				break;
			}
		}

		if (task != null) {
			task.getScheduling().setEnabled(guiTask.getScheduling().isEnabled());
			if (guiTask.getScheduling().isSimple()) {
				task.getScheduling().setMode(TaskTrigger.MODE_SIMPLE);
				task.getScheduling().setDelay(guiTask.getScheduling().getDelay() * 1000);
				task.getScheduling().setInterval(guiTask.getScheduling().getInterval() * 1000);
				task.getScheduling().setIntervalSeconds(guiTask.getScheduling().getInterval());
				guiTask.setSchedulingLabel(
						I18N.message("each", locale) + " " + task.getScheduling().getIntervalSeconds() + " "
								+ I18N.message(SECONDS, locale).toLowerCase());
			} else {
				task.getScheduling().setMode(TaskTrigger.MODE_CRON);

				InfoServiceImpl service = new InfoServiceImpl();
				service.getCronDescription(guiTask.getScheduling().getCronExpression(), locale);

				task.getScheduling().setCronExpression(guiTask.getScheduling().getCronExpression());
				guiTask.setSchedulingLabel(task.getScheduling().getCronExpression());
			}
			task.getScheduling().setMaxLength(guiTask.getScheduling().getMaxLength());

			task.setSendActivityReport(guiTask.isSendActivityReport());
			task.setReportRecipients(guiTask.getReportRecipients().stream().map(u -> Long.toString(u.getId()))
					.collect(Collectors.joining(",")));

			saveTask(task);
		}

		return guiTask;
	}

	private void saveTask(Task tsk) {
		try {
			tsk.save();
		} catch (IOException | ParseException e) {
			log.error(e.getMessage(), e);
		}
	}

	@Override
	public void startTask(String taskName) {
		TaskManager manager = Context.get().getBean(TaskManager.class);

		for (Task task : manager.getTasks()) {
			if (task.getName().equals(taskName)) {
				Thread thread = new Thread(task);
				thread.start();
				break;
			}
		}
	}

	@Override
	public void stopTask(String taskName) {
		TaskManager manager = Context.get().getBean(TaskManager.class);
		for (Task task : manager.getTasks()) {
			if (task.getName().equals(taskName)) {
				task.interrupt();
				break;
			}
		}
	}

	@Override
	public void setGUILanguageStatus(String language, boolean active) throws ServerException {
		Session session = validateSession();

		ContextProperties conf = Context.get().getProperties();
		conf.setProperty(session.getTenantName() + ".lang." + language + ".gui", active ? "enabled" : "disabled");
		try {
			conf.write();
		} catch (IOException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public void confirmUpdate() throws ServerException {
		Session session = validateSession();

		ContextProperties conf = Context.get().getProperties();
		String prevRunLevel = conf.getProperty("runlevel.back", RunLevel.DEFAULT.toString());
		conf.setProperty("runlevel", prevRunLevel);
		try {
			conf.write();
			log.info("Update confirmed");
		} catch (IOException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public void restart() throws ServerException {
		validateSession();

		try {
			log.warn("Alerting the connected users about the shutdown");
			List<Session> sessions = SessionManager.get().getSessions();
			WebsocketTool websocket = new WebsocketTool();
			for (Session session : sessions)
				websocket.showMessage(session, I18N.message("systemisshuttingdown", session.getUser().getLocale()),
						"warn");
		} catch (Exception e) {
			log.warn(e.getMessage());
		}

		try {
			SecurityServiceImpl secService = new SecurityServiceImpl();
			secService.logout();
		} catch (Exception e) {
			log.warn(e.getMessage());
		}

		ContextProperties config = Context.get().getProperties();
		File restartFile = new File(config.getProperty("LDOCHOME") + "/updates/restart");
		if (restartFile.exists())
			FileUtils.deleteQuietly(restartFile);
		restartFile.getParentFile().mkdirs();

		try {
			if (!restartFile.createNewFile())
				throw new ServerException("Unable to write file " + restartFile.getAbsolutePath());
		} catch (IOException e) {
			throw new ServerException("Unable to write file " + restartFile.getAbsolutePath());
		}
	}

	@Override
	public List<GUIHistory> search(Long userId, Date from, Date till, int maxResult, String historySid,
			List<String> events, Long rootFolderId) throws ServerException {
		Session session = validateSession();

		int i = 0;
		StringBuilder query = new StringBuilder();
		for (String table : History.eventTables()) {
			String tableAlias = "A" + (i++);

			if (!query.isEmpty())
				query.append(" union ");
			query.append(
					"select A.ld_username, A.ld_event, A.ld_date, A.ld_filename, A.ld_folderid, A.ld_path, A.ld_sessionid, A.ld_docid, A.ld_userid, A.ld_ip as ip, A.ld_userlogin, A.ld_comment, A.ld_reason, A.ld_device, A.ld_geolocation, A.ld_keylabel from TABLE A where A.ld_tenantid = "
					.replace("TABLE", table).replace("A", tableAlias) + session.getTenantId());
			appendUserCondition(tableAlias, userId, query);
			appendSessionCondition(tableAlias, historySid, query);
			appendDatesCondition(tableAlias, from, till, query);
			appendFolderCondition(tableAlias, rootFolderId, query);
			appendEventsCondition(tableAlias, events, query);
		}

		query.append(" order by 3 desc ");

		try {
			return executeQuery(query.toString(), maxResult, session);
		} catch (PersistenceException e) {
			return throwServerException(session, log, e);
		}
	}

	private void appendEventsCondition(String tableAlias, List<String> events, StringBuilder query) {
		if (CollectionUtils.isNotEmpty(events)) {
			query.append(" and (");
			query.append(events.stream().map(e -> " " + tableAlias + ".ld_event = '" + SqlUtil.doubleQuotes(e) + "'")
					.collect(Collectors.joining(" or ")));
			query.append(" ) ");
		}
	}

	private void appendDatesCondition(String tableAlias, Date from, Date till, StringBuilder query) {
		if (from != null) {
			query.append(AND + tableAlias + ".ld_date > '" + new Timestamp(from.getTime()) + "' ");
		}
		if (till != null) {
			query.append(AND + tableAlias + ".ld_date < '" + new Timestamp(till.getTime()) + "' ");
		}
	}

	private void appendSessionCondition(String tableAlias, String historySid, StringBuilder query) {
		if (historySid != null && StringUtils.isNotEmpty(historySid))
			query.append(AND + tableAlias + ".ld_sessionid='" + historySid + "' ");
	}

	private void appendUserCondition(String tableAlias, Long userId, StringBuilder query) {
		if (userId != null)
			query.append(AND + tableAlias + ".ld_userid = " + userId);
	}

	private List<GUIHistory> executeQuery(String query, int maxResult, Session session) throws PersistenceException {
		DocumentHistoryDAO dao = Context.get().getBean(DocumentHistoryDAO.class);
		return dao.query(query, new RowMapper<GUIHistory>() {

			@Override
			public GUIHistory mapRow(ResultSet rs, int row) throws SQLException {
				GUIHistory history = new GUIHistory();
				history.setTenant(session.getTenantName());
				history.setTenantId(session.getTenantId());
				history.setUsername(rs.getString(1));
				history.setEvent(rs.getString(2));
				history.setDate(SqlUtil.getColumnDateValue(rs, 3));
				history.setFileName(rs.getString(4));
				history.setFolderId(rs.getLong(5));
				history.setPath(rs.getString(6));
				history.setSessionId(rs.getString(7));
				history.setDocId(rs.getLong(8));
				history.setUserId(rs.getLong(9));
				history.setIp(rs.getString(10));
				history.setUserLogin(rs.getString(11));
				history.setComment(rs.getString(12));
				history.setReason(rs.getString(13));
				history.setDevice(rs.getString(14));
				history.setGeolocation(rs.getString(15));
				history.setKeyLabel(rs.getString(16));

				if (history.getFileName() != null && history.getDocId() != 0L)
					history.setIcon(IconSelector.selectIcon(history.getFileName()));
				else if (history.getFileName() != null && history.getDocId() == 0L && history.getFolderId() != 0L)
					history.setIcon("folder");

				return history;
			}
		}, maxResult);
	}

	private void appendFolderCondition(String tableAlias, Long rootFolderId, StringBuilder query) {
		if (rootFolderId == null)
			return;

		StringBuilder folderPredicate = new StringBuilder();
		FolderDAO fDao = Context.get().getBean(FolderDAO.class);
		Collection<Long> tree = fDao.findFolderIdInTree(rootFolderId, false);
		if (fDao.isOracle()) {
			/*
			 * In Oracle The limit of 1000 elements applies to sets of single
			 * items: (x) IN ((1), (2), (3), ...). There is no limit if the sets
			 * contain two or more items: (x, 0) IN ((1,0), (2,0), (3,0), ...):
			 */
			folderPredicate.append(" and (" + tableAlias + ".ld_folderid,0) in ( ");
			folderPredicate
					.append(tree.stream().map(id -> "(" + Long.toString(id) + ",0)").collect(Collectors.joining(",")));
			folderPredicate.append(" )");
		} else {
			folderPredicate.append(AND + tableAlias + ".ld_folderid in ( ");
			folderPredicate.append(tree.stream().map(id -> Long.toString(id)).collect(Collectors.joining(",")));
			folderPredicate.append(" )");
		}
		query.append(folderPredicate.toString());
	}

	@Override
	public List<GUIHistory> searchApiCalls(Long userId, Date from, Date till, String callSid, String protocol,
			String uri, int maxResult) throws ServerException {
		Session session = validateSession();

		try {
			TenantDAO dao = Context.get().getBean(TenantDAO.class);
			Map<Long, String> tenants = dao.findAll().stream()
					.collect(Collectors.toMap(Tenant::getId, Tenant::getName));
			tenants.put(Tenant.SYSTEM_ID, "system");

			// Search in the document/folder history
			StringBuilder query = new StringBuilder(
					"select ld_username, ld_date, ld_path, ld_sessionid, ld_userid, ld_ip as ip, ld_userlogin, ld_comment, ld_device, ld_geolocation, ld_protocol, ld_tenantId, ld_keylabel from ld_webservicecall where 1 = 1 ");
			if (userId != null)
				query.append(" and ld_userid = " + userId);
			if (callSid != null && StringUtils.isNotEmpty(callSid))
				query.append(" and ld_sessionid='" + callSid + "' ");
			if (from != null) {
				query.append(" and ld_date > '" + new Timestamp(from.getTime()) + "' ");
			}
			if (till != null) {
				query.append(" and ld_date < '" + new Timestamp(till.getTime()) + "' ");
			}
			if (protocol != null) {
				query.append(" and ld_protocol = '" + protocol + "' ");
			}

			if (session.getTenantId() != Tenant.DEFAULT_ID)
				query.append(" and ld_tenantid = " + session.getTenantId());

			query.append(" order by ld_date desc ");

			return dao.query(query.toString(), new RowMapper<GUIHistory>() {

				@Override
				public GUIHistory mapRow(ResultSet rs, int arg1) throws SQLException {
					GUIHistory history = new GUIHistory();
					history.setUsername(rs.getString(1));
					history.setDate(SqlUtil.getColumnDateValue(rs, 2));
					history.setPath(rs.getString(3));
					history.setSessionId(rs.getString(4));
					history.setUserId(rs.getLong(5));
					history.setIp(rs.getString(6));
					history.setUserLogin(rs.getString(7));
					history.setComment(rs.getString(8));
					history.setDevice(rs.getString(9));
					history.setGeolocation(rs.getString(10));
					history.setProtocol(rs.getString(11));
					history.setTenantId(rs.getLong(12));
					history.setTenant(tenants.get(history.getTenantId()));
					history.setKeyLabel(rs.getString(13));

					return history;
				}

			}, maxResult);
		} catch (PersistenceException e) {
			return throwServerException(session, log, e);
		}
	}

	@Override
	public void unscheduleJobs(List<GUIValue> jobs) throws ServerException {
		Session session = validateSession();
		JobManager jobManager = Context.get().getBean(JobManager.class);
		for (GUIValue trigger : jobs)
			try {
				jobManager.unscheduleTrigger(trigger.getCode(), trigger.getValue());
			} catch (SchedulerException e) {
				throwServerException(session, log, e);
			}
	}

	private File getPluginArchive(String pluginId) {
		String location = PluginRegistry.getInstance().getPlugin(pluginId).getLocation().toString().substring(9);
		return new File(location.substring(0, location.lastIndexOf('!')));
	}

	@Override
	public void uninstallPlugin(String pluginId) throws ServerException {
		Session session = validateSession();

		if (!session.getUser().isMemberOf(Group.GROUP_ADMIN) || session.getTenantId() != Tenant.DEFAULT_ID)
			throw new AccessDeniedException();

		PluginRegistry pluginRegistry = PluginRegistry.getInstance();

		/*
		 * Launch the plugin's install
		 */
		PluginDescriptor plugin = pluginRegistry.getPlugin(pluginId);
		if (plugin == null)
			return;

		if (plugin.getAttribute("removable") == null || "false".equals(plugin.getAttribute("removable").getValue()))
			throw new ServerException("The plugin " + pluginId + " cannot be uninstalled");

		File pluginJarFile = getPluginArchive(pluginId);

		pluginRegistry.getManager().deactivatePlugin(pluginId);

		FileUtil.delete(PluginRegistry.getPluginHome(pluginId));
		FileUtil.delete(pluginJarFile);
		if (pluginJarFile.exists())
			try {
				FileUtils.forceDelete(pluginJarFile);
			} catch (IOException t) {
				// Nothing to do
			}

		try {
			if (pluginJarFile.exists()) {
				if (new Exec().isWindows())
					Runtime.getRuntime().exec(
							new String[] { "cmd.exe", "/c", "del /F /Q \"" + pluginJarFile.getAbsolutePath() + "\"" });
				else
					Runtime.getRuntime().exec(new String[] { "rm", "-rf", pluginJarFile.getAbsolutePath() });
			}
		} catch (IOException e) {
			throwServerException(session, log, e);
		}

		if (pluginJarFile.exists())
			throw new ServerException("Cannot remove plugin file " + pluginJarFile.getAbsolutePath()
					+ ". Please stop the application and delete that file manually.");
	}

	@Override
	public void initializePlugin(String pluginId) throws ServerException {
		Session session = validateSession();

		if (!session.getUser().isMemberOf(Group.GROUP_ADMIN) || session.getTenantId() != Tenant.DEFAULT_ID)
			throw new AccessDeniedException();

		try {
			/*
			 * Launch the plugin's install
			 */
			PluginDescriptor plugin = PluginRegistry.getInstance().getPlugin(pluginId);
			if (plugin.getPluginClassName() != null) {
				LogicalDOCPlugin pluginInstance = (LogicalDOCPlugin) Class.forName(plugin.getPluginClassName())
						.getDeclaredConstructor().newInstance();
				pluginInstance.install();
			}

			/*
			 * Initialize the database
			 */
			ContextProperties config = Context.get().getProperties();
			PluginDbInit dbInit = new PluginDbInit();
			dbInit.setDbms(config.getProperty("jdbc.dbms"));
			dbInit.setDriver(config.getProperty("jdbc.driver"));
			dbInit.setUrl(config.getProperty("jdbc.url"));
			dbInit.setUsername(config.getProperty("jdbc.username"));
			dbInit.setPassword(config.getProperty("jdbc.password"));

			if (dbInit.testConnection()) {
				// connection success
				dbInit.init(Set.of(pluginId));
			} else {
				// connection failure
				log.debug("connection failure");
				throw new ServerException("Database Connection failure.");
			}
		} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException
				| NoSuchMethodException | ClassNotFoundException | PluginException | ServerException | SQLException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public void installPlugin() throws ServerException {
		Session session = validateSession();

		if (!session.getUser().isMemberOf(Group.GROUP_ADMIN) || session.getTenantId() != Tenant.DEFAULT_ID)
			throw new AccessDeniedException();

		Map<String, File> uploadedFilesMap = UploadServlet.getUploads(session.getSid());
		if (uploadedFilesMap == null || uploadedFilesMap.size() < 1)
			throw new ServerException("Cannot find a plugin package to install");

		try (ZipUtil zipUtil = new ZipUtil();) {
			File pluginPackage = uploadedFilesMap.values().iterator().next();
			ContextProperties config = Context.get().getProperties();
			File rootFolder;
			if (getThreadLocalRequest() != null)
				rootFolder = new File(getThreadLocalRequest().getSession().getServletContext().getRealPath("/"));
			else
				rootFolder = defaultWebappRootFolder;

			String pluginId = null;
			String pluginVersion = null;
			String pluginJar = null;

			try (InputStream pluginStream = zipUtil.getEntryStream(pluginPackage, "/plugin.xml")) {
				if (pluginStream == null)
					throw new ServerException("The plugin package does not include the descriptor plugin.xml");

				PluginDescriptorConfigurator pd = new PluginDescriptorConfigurator(pluginStream);
				pluginId = pd.getId();
				pluginVersion = pd.getVersion();

				PluginRegistry pluginRegistry = PluginRegistry.getInstance();
				Set<String> dependencies = pd.getDependencies();
				for (String dependency : dependencies) {
					PluginDescriptor dep = pluginRegistry.getPlugin(dependency);
					if (dep == null)
						throw new ServerException("The dependency from plugin " + dependency + " cannot be resolved");
				}

				pluginJar = pluginId + "-" + pluginVersion + "-plugin.jar";
			}

			List<String> entries = zipUtil.listEntries(pluginPackage);
			if (!entries.contains("WEB-INF/lib/" + pluginJar))
				throw new ServerException(
						"The plugin package does not include the plugin file WEB-INF/lib/" + pluginJar);

			log.info("Unpacking plugin package {} into {}", pluginPackage.getName(), rootFolder.getAbsolutePath());
			zipUtil.unzip(pluginPackage, rootFolder);

			// Delete the plugin.xml file
			FileUtils.deleteQuietly(new File(rootFolder, "plugin.xml"));

			File pluginHome = PluginRegistry.getPluginHome(pluginId);
			if (pluginHome.exists()) {
				FileUtil.delete(pluginHome);
				log.info("Deleted existing plugin home {}", pluginHome.getAbsolutePath());
			}

			File libFolder = new File(new File(rootFolder, "WEB-INF"), "lib");
			File pluginJarFile = new File(libFolder, pluginJar);

			/*
			 * Append the plugin jar in the classpath
			 */
			appendPluginJarInClasspath(pluginJarFile);

			/*
			 * Initialize the plugin
			 */
			PluginRegistry pluginRegistry = PluginRegistry.getInstance();
			pluginRegistry.init(libFolder.getAbsolutePath());
			initializePlugin(pluginId);

			/*
			 * Copy the plugin archive as .installed so it will be maintained
			 * over the updates
			 */
			File pluginsDir = new File(config.getProperty("conf.plugindir"));
			File targetFile = new File(pluginsDir, pluginId + "-" + pluginVersion + "-plugin.zip.installed");
			log.info("Copying plugin package {} into {}", pluginPackage.getName(), targetFile.getAbsolutePath());
			FileUtil.copyFile(pluginPackage, targetFile);

			if (pluginRegistry.isRestartRequired())
				ApplicationListener.restartRequired();
		} catch (ServerException | IOException | NoSuchMethodException | IllegalArgumentException
				| InvocationTargetException | PluginException e) {
			throwServerException(session, log, e);
		} finally {
			UploadServlet.cleanUploads(session.getSid());
		}
	}

	private void appendPluginJarInClasspath(File pluginJarFile)
			throws NoSuchMethodException, InvocationTargetException, MalformedURLException {
		final ClassLoader sysloader = this.getClass().getClassLoader();
		final Class<URLClassLoader> sysclass = URLClassLoader.class;
		final Method method = sysclass.getDeclaredMethod("addURL", URL.class);

		try {
			method.invoke(sysloader, pluginJarFile.toURI().toURL());
		} catch (IllegalAccessException iae) {
			log.warn(iae.getMessage(), iae);
		}
	}

	@Override
	public List<GUIValue> getPlugins() throws ServerException {
		validateSession();

		Collection<PluginDescriptor> descriptors = PluginRegistry.getInstance().getPlugins();
		List<GUIValue> plugins = new ArrayList<>();
		for (PluginDescriptor descriptor : descriptors) {
			GUIValue plugin = new GUIValue();
			plugin.setCode(descriptor.getId());
			plugin.setValue(descriptor.getVersion().toString());
			plugins.add(plugin);
		}

		if (plugins.size() > 1) {
			// Sort by ascending date and number
			Collections.sort(plugins, (c1, c2) -> {
				if (c1.getCode() != null && c2.getCode() != null) {
					int compare = c1.getCode().compareTo(c2.getCode());
					if (compare != 0)
						return compare;
				}
				return c1.getValue().compareTo(c2.getValue());
			});
		}

		return plugins;
	}

	@Override
	public void saveLogger(String name, String level, boolean additivity) throws ServerException {
		Session session = validateSession();
		if (session.getTenantId() != Tenant.DEFAULT_ID)
			throw new AccessDeniedException("Not enough permissions");
		checkMenu(Menu.LOGS);

		LogConfigurator conf = new LogConfigurator();
		if ("root".equals(name))
			conf.setRootLevel(level);
		else
			conf.setLogger(name, additivity, StringUtils.defaultIfEmpty(level, "info"), null);

		conf.write();
		conf.initializeLogging();
	}

	@Override
	public void removeLogger(String name) throws ServerException {
		Session session = validateSession();
		if (session.getTenantId() != Tenant.DEFAULT_ID || LogDataServlet.isLoggerReserved(name))
			throw new AccessDeniedException("Not enough permissions");
		checkMenu(Menu.LOGS);

		LogConfigurator conf = new LogConfigurator();
		conf.removeLogger(name);
		conf.write();
		conf.initializeLogging();
	}
}