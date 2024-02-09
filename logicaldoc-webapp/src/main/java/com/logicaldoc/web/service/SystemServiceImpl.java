package com.logicaldoc.web.service;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLClassLoader;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
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

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.java.plugin.registry.PluginDescriptor;
import org.quartz.SchedulerException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.RowMapper;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.RunLevel;
import com.logicaldoc.core.SystemInfo;
import com.logicaldoc.core.dbinit.PluginDbInit;
import com.logicaldoc.core.document.DocumentHistoryDAO;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.generic.Generic;
import com.logicaldoc.core.generic.GenericDAO;
import com.logicaldoc.core.job.JobManager;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.TenantDAO;
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
import com.logicaldoc.util.config.PluginDescriptorConfigurator;
import com.logicaldoc.util.exec.Exec;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.io.ZipUtil;
import com.logicaldoc.util.plugin.LogicalDOCPlugin;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.plugin.PluginRegistry;
import com.logicaldoc.util.sql.SqlUtil;
import com.logicaldoc.web.UploadServlet;
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
			return (Boolean) throwServerException(session, log, e);
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
			return (Boolean) throwServerException(session, log, e);
		}
	}

	@Override
	public GUIParameter[][] getStatistics(String locale) throws ServerException {
		Session session = validateSession();

		GenericDAO genDao = (GenericDAO) Context.get().getBean(GenericDAO.class);

		GUIParameter[][] parameters = new GUIParameter[5][9];

		/*
		 * Repository statistics
		 */
		Generic gen = genDao.findByAlternateKey(StatsCollector.STAT, TRASH, null, session.getTenantId());
		GUIParameter trashSize = new GUIParameter();
		trashSize.setName(TRASH);
		setLongValue(gen, trashSize);
		parameters[0][1] = trashSize;

		gen = genDao.findByAlternateKey(StatsCollector.STAT, "docdir", null, session.getTenantId());
		GUIParameter docDirSize = new GUIParameter();
		docDirSize.setName("documents");
		setLongValue(gen, docDirSize);
		docDirSize
				.setValue(Long.toString(Long.parseLong(docDirSize.getValue()) - Long.parseLong(trashSize.getValue())));
		parameters[0][0] = docDirSize;

		gen = genDao.findByAlternateKey(StatsCollector.STAT, "userdir", null, Tenant.SYSTEM_ID);
		GUIParameter userDirSize = new GUIParameter();
		userDirSize.setName("users");
		setLongValue(gen, userDirSize);
		parameters[0][2] = userDirSize;

		gen = genDao.findByAlternateKey(StatsCollector.STAT, "indexdir", null, Tenant.SYSTEM_ID);
		GUIParameter indexDirSize = new GUIParameter();
		indexDirSize.setName("fulltextindex");
		setLongValue(gen, indexDirSize);
		parameters[0][3] = indexDirSize;

		gen = genDao.findByAlternateKey(StatsCollector.STAT, "importdir", null, Tenant.SYSTEM_ID);
		GUIParameter importDirSize = new GUIParameter();
		importDirSize.setName("iimport");
		setLongValue(gen, importDirSize);
		parameters[0][4] = importDirSize;

		gen = genDao.findByAlternateKey(StatsCollector.STAT, "exportdir", null, Tenant.SYSTEM_ID);
		GUIParameter exportDirSize = new GUIParameter();
		exportDirSize.setName("eexport");
		setLongValue(gen, exportDirSize);
		parameters[0][5] = exportDirSize;

		gen = genDao.findByAlternateKey(StatsCollector.STAT, "plugindir", null, Tenant.SYSTEM_ID);
		GUIParameter pluginsDirSize = new GUIParameter();
		pluginsDirSize.setName("plugins");
		setLongValue(gen, pluginsDirSize);
		parameters[0][6] = pluginsDirSize;

		gen = genDao.findByAlternateKey(StatsCollector.STAT, "dbdir", null, Tenant.SYSTEM_ID);
		GUIParameter dbDirSize = new GUIParameter();
		dbDirSize.setName("database");
		setLongValue(gen, dbDirSize);
		parameters[0][7] = dbDirSize;

		gen = genDao.findByAlternateKey(StatsCollector.STAT, "logdir", null, Tenant.SYSTEM_ID);
		GUIParameter logsDirSize = new GUIParameter();
		logsDirSize.setName("logs");
		setLongValue(gen, logsDirSize);
		parameters[0][8] = logsDirSize;

		/*
		 * Documents statistics
		 */
		gen = genDao.findByAlternateKey(StatsCollector.STAT, "notindexeddocs", null, session.getTenantId());
		GUIParameter notIndexed = new GUIParameter();
		notIndexed.setName("notindexed");
		setLongValue(gen, notIndexed);
		parameters[1][0] = notIndexed;

		gen = genDao.findByAlternateKey(StatsCollector.STAT, "notindexabledocs", null, session.getTenantId());
		GUIParameter notIndexableDocs = new GUIParameter();
		notIndexableDocs.setName("notindexabledocs");
		notIndexableDocs.setLabel("notindexable");
		setLongValue(gen, notIndexableDocs);
		parameters[1][1] = notIndexableDocs;

		gen = genDao.findByAlternateKey(StatsCollector.STAT, "indexeddocs", null, session.getTenantId());
		GUIParameter indexed = new GUIParameter();
		indexed.setName("indexed");
		setLongValue(gen, indexed);
		parameters[1][2] = indexed;

		gen = genDao.findByAlternateKey(StatsCollector.STAT, "deleteddocs", null, session.getTenantId());
		GUIParameter deletedDocs = new GUIParameter();
		deletedDocs.setName("docstrash");
		deletedDocs.setLabel(TRASH);
		setLongValue(gen, deletedDocs);
		parameters[1][3] = deletedDocs;

		gen = genDao.findByAlternateKey(StatsCollector.STAT, "archiveddocs", null, session.getTenantId());
		GUIParameter archivedDocs = new GUIParameter();
		archivedDocs.setName("archiveddocs");
		archivedDocs.setLabel("archiveds");
		setLongValue(gen, archivedDocs);
		parameters[1][4] = archivedDocs;

		/*
		 * Pages statistics
		 */
		gen = genDao.findByAlternateKey(StatsCollector.STAT, "notindexedpages", null, session.getTenantId());
		GUIParameter notIndexedPages = new GUIParameter();
		notIndexedPages.setName("notindexed");
		setLongValue(gen, notIndexedPages);
		parameters[2][0] = notIndexedPages;

		gen = genDao.findByAlternateKey(StatsCollector.STAT, "notindexablepages", null, session.getTenantId());
		GUIParameter notIndexablePages = new GUIParameter();
		notIndexablePages.setName("notindexablepages");
		notIndexablePages.setLabel("notindexable");
		setLongValue(gen, notIndexablePages);
		parameters[2][1] = notIndexableDocs;

		gen = genDao.findByAlternateKey(StatsCollector.STAT, "indexedpages", null, session.getTenantId());
		GUIParameter indexedPages = new GUIParameter();
		indexedPages.setName("indexed");
		setLongValue(gen, indexedPages);
		parameters[2][2] = indexedPages;

		gen = genDao.findByAlternateKey(StatsCollector.STAT, "deletedpages", null, session.getTenantId());
		GUIParameter deletedPages = new GUIParameter();
		deletedPages.setName("pagestrash");
		deletedPages.setLabel(TRASH);
		setLongValue(gen, deletedPages);
		parameters[2][3] = deletedPages;

		gen = genDao.findByAlternateKey(StatsCollector.STAT, "archivedpages", null, session.getTenantId());
		GUIParameter archivedPages = new GUIParameter();
		archivedPages.setName("archivedpages");
		archivedPages.setLabel("archiveds");
		setLongValue(gen, archivedPages);
		parameters[2][4] = archivedPages;

		/*
		 * Folders statistics
		 */
		gen = genDao.findByAlternateKey(StatsCollector.STAT, "withdocs", null, session.getTenantId());
		GUIParameter notEmptyFolders = new GUIParameter();
		notEmptyFolders.setName("withdocs");
		setLongValue(gen, notEmptyFolders);
		parameters[3][0] = notEmptyFolders;

		gen = genDao.findByAlternateKey(StatsCollector.STAT, "empty", null, session.getTenantId());
		GUIParameter emptyFolders = new GUIParameter();
		emptyFolders.setName("empty");
		setLongValue(gen, emptyFolders);
		parameters[3][1] = emptyFolders;

		gen = genDao.findByAlternateKey(StatsCollector.STAT, "deletedfolders", null, session.getTenantId());
		GUIParameter deletedFolders = new GUIParameter();
		deletedFolders.setName("folderstrash");
		deletedFolders.setLabel(TRASH);
		setLongValue(gen, deletedFolders);
		parameters[3][2] = deletedFolders;

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
		parameters[4][0] = lastrun;

		return parameters;
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
		TaskManager manager = (TaskManager) Context.get().getBean(TaskManager.class);
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
		UserDAO dao = (UserDAO) Context.get().getBean(UserDAO.class);
		if (task.getReportRecipients() != null) {
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
	public GUITask[] loadTasks(String locale) throws ServerException {
		validateSession();

		TaskManager manager = (TaskManager) Context.get().getBean(TaskManager.class);
		GUITask[] tasks = new GUITask[manager.getTasks().size()];

		int i = 0;
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

			tasks[i++] = task;
		}

		return tasks;

	}

	@Override
	public GUITask saveTask(GUITask task, String locale) throws ServerException {
		validateSession();

		TaskManager manager = (TaskManager) Context.get().getBean(TaskManager.class);
		Task tsk = null;
		for (Task t : manager.getTasks()) {
			if (t.getName().equals(task.getName())) {
				tsk = t;
				break;
			}
		}

		if (tsk != null) {
			tsk.getScheduling().setEnabled(task.getScheduling().isEnabled());
			if (task.getScheduling().isSimple()) {
				tsk.getScheduling().setMode(TaskTrigger.MODE_SIMPLE);
				tsk.getScheduling().setDelay(task.getScheduling().getDelay() * 1000);
				tsk.getScheduling().setInterval(task.getScheduling().getInterval() * 1000);
				tsk.getScheduling().setIntervalSeconds(task.getScheduling().getInterval());
				task.setSchedulingLabel(I18N.message("each", locale) + " " + tsk.getScheduling().getIntervalSeconds()
						+ " " + I18N.message(SECONDS, locale).toLowerCase());
			} else {
				tsk.getScheduling().setMode(TaskTrigger.MODE_CRON);

				InfoServiceImpl service = new InfoServiceImpl();
				service.getCronDescription(task.getScheduling().getCronExpression(), locale);

				tsk.getScheduling().setCronExpression(task.getScheduling().getCronExpression());
				task.setSchedulingLabel(tsk.getScheduling().getCronExpression());
			}
			tsk.getScheduling().setMaxLength(task.getScheduling().getMaxLength());

			tsk.setSendActivityReport(task.isSendActivityReport());
			StringBuilder sb = new StringBuilder();
			for (GUIUser user : task.getReportRecipients()) {
				if (sb.length() > 0)
					sb.append(",");
				sb.append(user.getId());
			}
			tsk.setReportRecipients(sb.toString());

			saveTask(tsk);
		}

		return task;
	}

	private void saveTask(Task tsk) {
		try {
			tsk.save();
		} catch (IOException | ParseException e) {
			log.error(e.getMessage(), e);
		}
	}

	@Override
	public boolean startTask(String taskName) {
		TaskManager manager = (TaskManager) Context.get().getBean(TaskManager.class);

		for (Task task : manager.getTasks()) {
			if (task.getName().equals(taskName)) {
				Thread thread = new Thread(task);
				thread.start();
				break;
			}
		}

		return true;
	}

	@Override
	public boolean stopTask(String taskName) {
		TaskManager manager = (TaskManager) Context.get().getBean(TaskManager.class);
		for (Task task : manager.getTasks()) {
			if (task.getName().equals(taskName)) {
				task.interrupt();
				break;
			}
		}

		return true;
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
	public GUIHistory[] search(Long userId, Date from, Date till, int maxResult, String historySid, String[] events,
			Long rootFolderId) throws ServerException {
		Session session = validateSession();

		// Search in the document/folder history
		StringBuilder query = new StringBuilder(
				"select A.ld_username, A.ld_event, A.ld_date, A.ld_filename, A.ld_folderid, A.ld_path, A.ld_sessionid, A.ld_docid, A.ld_userid, A.ld_ip as ip, A.ld_userlogin, A.ld_comment, A.ld_reason, A.ld_device, A.ld_geolocation from ld_history A where A.ld_tenantid = "
						+ session.getTenantId());
		appendUserCondition("A", userId, query);
		appendSessionCondition("A", historySid, query);
		appendDatesCondition("A", from, till, query);
		appendFolderCondition("A", rootFolderId, query);
		appendEventsCondition("A", events, query);

		// Search in the folder history
		query.append(
				" union select B.ld_username, B.ld_event, B.ld_date, B.ld_filename, B.ld_folderid, B.ld_path, B.ld_sessionid, B.ld_docid, B.ld_userid, B.ld_ip as ip, B.ld_userlogin, B.ld_comment, B.ld_reason, B.ld_device, B.ld_geolocation from ld_folder_history B where B.ld_tenantid = "
						+ session.getTenantId());
		appendUserCondition("B", userId, query);
		appendSessionCondition("B", historySid, query);
		appendDatesCondition("B", from, till, query);
		appendFolderCondition("B", rootFolderId, query);
		appendEventsCondition("B", events, query);

		// Search in the user history
		if (rootFolderId == null) {
			query.append(
					" union select C.ld_username, C.ld_event, C.ld_date, null, null, null, C.ld_sessionid, null, C.ld_userid, C.ld_ip as ip, C.ld_userlogin, C.ld_comment, C.ld_reason, C.ld_device, C.ld_geolocation from ld_user_history C where C.ld_tenantid = "
							+ session.getTenantId());
			appendUserCondition("C", userId, query);
			appendSessionCondition("C", historySid, query);
			appendDatesCondition("C", from, till, query);
			appendEventsCondition("C", events, query);
		}

		if (Arrays.asList(SystemInfo.get().getFeatures()).contains("Feature_19")) {

			// Search in the workflow history
			query.append(
					" union select D.ld_username, D.ld_event, D.ld_date, null, null, null, D.ld_sessionid, D.ld_docid, D.ld_userid, '' as ip, D.ld_userlogin, D.ld_comment, D.ld_reason, D.ld_device, D.ld_geolocation from ld_workflowhistory D where D.ld_tenantid = "
							+ session.getTenantId());
			appendUserCondition("D", userId, query);
			appendSessionCondition("D", historySid, query);
			appendDatesCondition("D", from, till, query);
			appendFolderCondition("D", rootFolderId, query);
			appendEventsCondition("D", events, query);
		}

		if (Arrays.asList(SystemInfo.get().getFeatures()).contains("Feature_1")) {
			// Search in the workflow history
			query.append(
					" union select E.ld_username, E.ld_event, E.ld_date, E.ld_filename, E.ld_folderid, E.ld_path, null, E.ld_docid, E.ld_userid, null as ip, E.ld_userlogin, E.ld_comment, null, null, null  from ld_importfolder_history E where E.ld_tenantid = "
							+ session.getTenantId());
			appendUserCondition("E", userId, query);
			if (StringUtils.isNotEmpty(historySid))
				query.append(" and 1 = 2 ");
			appendDatesCondition("E", from, till, query);
			appendFolderCondition("E", rootFolderId, query);
			appendEventsCondition("E", events, query);
		}

		if (Arrays.asList(SystemInfo.get().getFeatures()).contains("Feature_3")) {
			// Search in the OCR history
			query.append(
					" union select F.ld_username, F.ld_event, F.ld_date, F.ld_filename, F.ld_folderid, F.ld_path, null, F.ld_docid, F.ld_userid, null as ip, F.ld_userlogin, F.ld_comment, null, null, null  from ld_ocr_history F where F.ld_tenantid = "
							+ session.getTenantId());
			appendUserCondition("F", userId, query);
			if (StringUtils.isNotEmpty(historySid))
				query.append(" and 1 = 2 ");
			appendDatesCondition("F", from, till, query);
			appendFolderCondition("F", rootFolderId, query);
			appendEventsCondition("F", events, query);
		}

		query.append(" order by 3 desc ");

		try {
			List<GUIHistory> histories = executeQuery(query.toString(), maxResult, session);
			return histories.toArray(new GUIHistory[histories.size()]);
		} catch (PersistenceException e) {
			return (GUIHistory[]) throwServerException(session, log, e);
		}
	}

	private void appendEventsCondition(String tableAlias, String[] events, StringBuilder query) {
		if (events.length > 0) {
			query.append(" and (");
			query.append(
					Arrays.stream(events).map(e -> " " + tableAlias + ".ld_event = '" + SqlUtil.doubleQuotes(e) + "'")
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

	@SuppressWarnings("unchecked")
	private List<GUIHistory> executeQuery(String query, int maxResult, Session session) throws PersistenceException {
		DocumentHistoryDAO dao = (DocumentHistoryDAO) Context.get().getBean(DocumentHistoryDAO.class);
		return dao.query(query, new RowMapper<>() {

			@Override
			public GUIHistory mapRow(ResultSet rs, int arg1) throws SQLException {
				GUIHistory history = new GUIHistory();
				history.setTenant(session.getTenantName());
				history.setTenantId(session.getTenantId());
				history.setUsername(rs.getString(1));
				history.setEvent(rs.getString(2));

				if (rs.getObject(3) instanceof Timestamp || rs.getObject(3) instanceof LocalDateTime)
					history.setDate(rs.getTimestamp(3));
				else
					history.setDate(rs.getDate(3));

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
		FolderDAO fDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
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

	@SuppressWarnings("unchecked")
	@Override
	public GUIHistory[] searchApiCalls(Long userId, Date from, Date till, String callSid, String protocol, String uri,
			int maxResult) throws ServerException {
		Session session = validateSession();

		TenantDAO dao = (TenantDAO) Context.get().getBean(TenantDAO.class);
		Map<Long, String> tenants = dao.findAll().stream().collect(Collectors.toMap(Tenant::getId, Tenant::getName));
		tenants.put(Tenant.SYSTEM_ID, "system");

		// Search in the document/folder history
		StringBuilder query = new StringBuilder(
				"select ld_username, ld_date, ld_path, ld_sessionid, ld_userid, ld_ip as ip, ld_userlogin, ld_comment, ld_device, ld_geolocation, ld_protocol, ld_tenantId from ld_webservicecall where 1 = 1 ");
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

		try {
			List<GUIHistory> calls = dao.query(query.toString(), new RowMapper<GUIHistory>() {

				@Override
				public GUIHistory mapRow(ResultSet rs, int arg1) throws SQLException {
					GUIHistory history = new GUIHistory();
					history.setUsername(rs.getString(1));
					if (rs.getObject(2) instanceof Timestamp)
						history.setDate(rs.getTimestamp(2));
					else
						history.setDate(rs.getDate(2));

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

					return history;
				}

			}, maxResult);

			return calls.toArray(new GUIHistory[calls.size()]);
		} catch (PersistenceException e) {
			return (GUIHistory[]) throwServerException(session, log, e);
		}
	}

	@Override
	public void unscheduleJobs(GUIValue[] jobs) throws ServerException {
		Session session = validateSession();
		JobManager jobManager = (JobManager) Context.get().getBean(JobManager.class);
		for (GUIValue trigger : jobs)
			try {
				jobManager.unscheduleTrigger(trigger.getCode(), trigger.getValue());
			} catch (SchedulerException e) {
				throwServerException(session, log, e);
			}

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

		String pluginVersion = plugin.getVersion().toString();
		String pluginJar = pluginId + "-" + pluginVersion + "-plugin.jar";

		File libFolder = new File(getThreadLocalRequest().getSession().getServletContext().getRealPath("/WEB-INF/lib"));
		File pluginJarFile = new File(libFolder, pluginJar);

		pluginRegistry.getManager().deactivatePlugin(pluginId);

		FileUtil.strongDelete(PluginRegistry.getPluginHome(pluginId));
		FileUtil.strongDelete(pluginJarFile);
		if (pluginJarFile.exists())
			try {
				FileUtils.forceDelete(pluginJarFile);
			} catch (IOException t) {
				// Nothing to do
			}

		try {
			if (pluginJarFile.exists()) {
				if (new Exec().isWindows())
					Runtime.getRuntime().exec("cmd.exe /c \"del /F /Q \"" + pluginJarFile.getAbsolutePath() + "\"\"");
				else
					new Exec().exec("rm -rf \"" + pluginJarFile.getAbsolutePath() + "\"", null, null);
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
			LogicalDOCPlugin pluginInstance = (LogicalDOCPlugin) Class.forName(plugin.getPluginClassName())
					.getDeclaredConstructor().newInstance();
			pluginInstance.install();

			/*
			 * Initialize the database
			 */
			ContextProperties config = Context.get().getProperties();
			PluginDbInit init = new PluginDbInit();
			init.setDbms(config.getProperty("jdbc.dbms"));
			init.setDriver(config.getProperty("jdbc.driver"));
			init.setUrl(config.getProperty("jdbc.url"));
			init.setUsername(config.getProperty("jdbc.username"));
			init.setPassword(config.getProperty("jdbc.password"));

			if (init.testConnection()) {
				// connection success
				init.init(new String[] { pluginId });
			} else {
				// connection failure
				log.debug("connection failure");
				throw new ServerException("Database Connection failure.");
			}
		} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException
				| NoSuchMethodException | SecurityException | ClassNotFoundException | PluginException | ServerException
				| SQLException e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public void installPlugin() throws ServerException {
		Session session = validateSession();

		if (!session.getUser().isMemberOf(Group.GROUP_ADMIN) || session.getTenantId() != Tenant.DEFAULT_ID)
			throw new AccessDeniedException();

		Map<String, File> uploadedFilesMap = UploadServlet.getReceivedFiles(session.getSid());
		if (uploadedFilesMap == null || uploadedFilesMap.size() < 1)
			throw new ServerException("Cannot find a plugin package to install");

		try {
			File pluginPackage = uploadedFilesMap.values().iterator().next();
			ContextProperties config = Context.get().getProperties();
			File rootFolder = new File(getThreadLocalRequest().getSession().getServletContext().getRealPath("/"));

			String pluginId = null;
			String pluginVersion = null;
			String pluginJar = null;

			ZipUtil zipUtil = new ZipUtil();
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
			zipUtil.unzip(pluginPackage.getAbsolutePath(), rootFolder.getAbsolutePath());

			// Delete the plugin.xml file
			FileUtils.deleteQuietly(new File(rootFolder, "plugin.xml"));

			File pluginHome = PluginRegistry.getPluginHome(pluginId);
			if (pluginHome.exists()) {
				FileUtil.strongDelete(pluginHome);
				log.info("Deleted existing plugin home {}", pluginHome.getAbsolutePath());
			}

			File libFolder = new File(
					getThreadLocalRequest().getSession().getServletContext().getRealPath("/WEB-INF/lib"));
			File pluginJarFile = new File(libFolder, pluginJar);

			/*
			 * Append the plugin jar in the classpath
			 */
			final ClassLoader sysloader = this.getClass().getClassLoader();
			final Class<URLClassLoader> sysclass = URLClassLoader.class;
			final Method method = sysclass.getDeclaredMethod("addURL", URL.class);
			method.invoke(sysloader, pluginJarFile.toURI().toURL());

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
		} catch (ServerException | IOException | NoSuchMethodException | SecurityException | IllegalAccessException
				| IllegalArgumentException | InvocationTargetException | PluginException e) {
			throwServerException(session, log, e);
		} finally {
			UploadServlet.cleanReceivedFiles(session.getSid());
		}
	}

	@Override
	public GUIValue[] getPlugins() throws ServerException {
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

		return plugins.toArray(new GUIValue[0]);
	}
}