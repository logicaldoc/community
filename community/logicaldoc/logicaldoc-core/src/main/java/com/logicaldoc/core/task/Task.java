package com.logicaldoc.core.task;

import java.io.IOException;
import java.security.InvalidParameterException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.UUID;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.RunLevel;
import com.logicaldoc.core.automation.Automation;
import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailSender;
import com.logicaldoc.core.communication.Recipient;
import com.logicaldoc.core.lock.LockManager;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.core.system.SystemLoadMonitor;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;

/**
 * A task is a long running sequence of operations
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 3.5.0
 */
public abstract class Task implements Runnable {
	protected Logger log = LoggerFactory.getLogger(Task.class);

	public final static int STATUS_IDLE = 0;

	public final static int STATUS_RUNNING = 1;

	public final static int STATUS_STOPPING = 2;

	private int status = STATUS_IDLE;

	protected long size = 0;

	private long progress = 0;

	private TaskScheduling scheduling;

	private String name;

	// When becomes true, the processing must be terminated asap but gracefully
	// and leaving the system in a consistent state
	protected boolean interruptRequested = false;

	private List<TaskListener> taskListeners = Collections.synchronizedList(new ArrayList<TaskListener>());

	protected Throwable lastRunError = null;

	protected ContextProperties config;

	protected EMailSender sender = null;

	protected UserDAO userDao = null;

	protected boolean sendActivityReport = false;

	private String reportRecipients = null;

	protected String transactionId = null;

	protected LockManager lockManager;

	protected SystemLoadMonitor systemLoadMonitor;

	public Task(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	protected void setName(String name) {
		this.name = name;
	}

	private void setStatus(int status) {
		if (status != STATUS_IDLE && status != STATUS_RUNNING && status != STATUS_STOPPING)
			throw new InvalidParameterException("Invalid status  value");
		boolean needNotification = this.status != status;
		this.status = status;
		if (needNotification)
			for (TaskListener listener : taskListeners)
				listener.statusChanged(status);

	}

	/**
	 * Increments the progress by one and detects system overload.
	 */
	protected void next() {
		setProgress(progress + 1);

		// Reset the timeout
		if (lockManager != null)
			lockManager.get(getName(), transactionId);

		if (systemLoadMonitor != null) {
			boolean overload = false;

			Random random = new Random();
			while (systemLoadMonitor.isAverageCpuOverLoaded()) {
				if (overload == false) {
					overload = true;
					log.info("Execution paused because of system overload");
				}
				try {
					lockManager.get(getName(), transactionId);
				} catch (Throwable e) {
				}
				try {
					Thread.sleep((1 + random.nextInt(20)) * 1000);
				} catch (Throwable e) {
				}
			}

			if (overload)
				log.info("Execution resumed after system overload");
		}
	}

	protected void setProgress(long progress) {
		try {
			if (progress > size || progress < 0)
				return;

			boolean needNotification = this.progress != progress;
			this.progress = progress;
			if (needNotification)
				for (TaskListener listener : taskListeners)
					listener.progressChanged(progress);
		} catch (Throwable t) {
			// Nothing to do
		} finally {
			// Check it time was expired, and request interruption if the case
			if (getScheduling().isExpired())
				interrupt();
		}
	}

	@Override
	public void run() {
		if (!RunLevel.current().aspectEnabled("scheduledTasks")) {
			log.debug("Aspect Sceduled Tasks not enabled");
			return;
		}

		System.gc();
		if (!getScheduling().isEnabled()) {
			log.debug("Task " + getName() + " is disabled");
			return;
		}

		if (getStatus() != STATUS_IDLE) {
			log.debug("Task " + getName() + " is already running");
			return;
		}

		log.info("Task " + getName() + " started");
		interruptRequested = false;
		setStatus(STATUS_RUNNING);
		getScheduling().setPreviousFireTime(new Date());
		setProgress(0);
		lastRunError = null;

		try {
			/*
			 * Need to acquire the lock
			 */
			transactionId = UUID.randomUUID().toString();
			if (isConcurrent() || (lockManager != null && lockManager.get(getName(), transactionId)))
				runTask();
		} catch (Throwable t) {
			log.error("Error caught " + t.getMessage(), t);
			log.error("The task is stopped");
			lastRunError = t;
		} finally {
			// In any case release the lock
			try {
				lockManager.release(getName(), transactionId);
			} catch (Throwable t) {

			}

			setStatus(STATUS_IDLE);
			interruptRequested = false;
			saveWork();
			log.info("Task " + getName() + " finished");
			if (isSendActivityReport() && StringUtils.isNotEmpty(getReportRecipients()))
				notifyReport();
			transactionId = null;
		}
	}

	public void interrupt() {
		interruptRequested = true;
		setStatus(STATUS_STOPPING);
	}

	public boolean isInterrupted() {
		return getStatus() == STATUS_IDLE;
	}

	/**
	 * The the total size of the processing(number of units of work)
	 */
	public long getSize() {
		return size;
	}

	public void setSize(long size) {
		this.size = size;
	}

	/**
	 * The task status(one of STATUS_IDLE or STATUS_RUNNING)
	 */
	public int getStatus() {
		return status;
	}

	/**
	 * The current processing step
	 */
	public long getProgress() {
		return progress;
	}

	/**
	 * The percentage of completion(1-100)
	 */
	public int getCompletionPercentage() {
		if (isIndeterminate()) {
			if (getStatus() == STATUS_IDLE)
				return 0;
			else
				return 1;
		} else {
			if (size == 0)
				return 0;
			return (int) (Math.round(((double) progress / (double) size) * 100));
		}
	}

	/**
	 * Check if the task is currently running
	 */
	public boolean isRunning() {
		return status == STATUS_RUNNING;
	}

	/**
	 * Concrete implementations must insert here the code needed to save the
	 * elaboration state in a persistent storage
	 */
	public void saveWork() {
		// By default do nothing
	}

	/**
	 * Scheduling policies
	 */
	public TaskScheduling getScheduling() {
		if (scheduling == null) {
			scheduling = new TaskScheduling(getName());
			try {
				scheduling.load();
			} catch (Exception e) {
				log.error(e.getMessage());
			}
		}
		return scheduling;
	}

	public synchronized void addTaskListener(TaskListener listener) {
		if (!taskListeners.contains(listener))
			taskListeners.add(listener);
	}

	public synchronized void removeTaskListener(TaskListener listener) {
		if (taskListeners.contains(listener))
			taskListeners.remove(listener);
	}

	public void notifyReport() {
		StringTokenizer st = new StringTokenizer(getReportRecipients(), ", ;", false);

		// Iterate over tokens loading the user to be notified
		while (st.hasMoreTokens()) {
			String userId = st.nextToken();
			User recipient = userDao.findById(Long.parseLong(userId));
			if (recipient == null || StringUtils.isEmpty(recipient.getEmail()))
				continue;

			EMail email = new EMail();
			email.setHtml(1);
			email.setLocale(recipient.getLocale());

			// Prepare the mail recipient
			Set<Recipient> rec = new HashSet<Recipient>();
			Recipient r = new Recipient();
			r.setAddress(recipient.getEmail());
			r.setType(Recipient.TYPE_EMAIL);
			r.setMode(Recipient.MODE_EMAIL_TO);
			rec.add(r);
			email.setRecipients(rec);

			// Prepare the arguments for the template
			Map<String, Object> dictionary = new HashMap<String, Object>();
			dictionary.put(Automation.LOCALE, recipient.getLocale());
			dictionary.put("task", I18N.message("task.name." + name, recipient.getLocale()));
			dictionary.put("started", scheduling.getPreviousFireTime());
			dictionary.put("ended", new Date());
			dictionary.put("error", (lastRunError != null ? lastRunError.getMessage() : null));
			dictionary.put("report", prepareReport(recipient.getLocale()).replaceAll("\\n", "<br />"));

			// Send the email
			try {
				sender.send(email, "task.report", dictionary);
				log.info("Report sent to: " + recipient.getEmail());
			} catch (Exception e) {
				log.error(e.getMessage(), e);
			}
		}
	}

	/**
	 * Implementations may compose a locale specific report.
	 */
	protected String prepareReport(Locale locale) {
		return null;
	}

	/**
	 * Concrete implementations must override this method implementing their own
	 * processing logic.
	 * 
	 * @throws Exception If something goes wrong this exception is raised
	 */
	abstract protected void runTask() throws Exception;

	/**
	 * Concrete implementations must override this method declaring if the task
	 * is indeterminate. An indeterminate task is not able to compute it's time
	 * length
	 */
	abstract public boolean isIndeterminate();

	/**
	 * Concrete implementations must override this method declaring if the task
	 * supports multiple instances running concurrently.
	 */
	abstract public boolean isConcurrent();

	public String getReportRecipients() {
		return reportRecipients;
	}

	public ContextProperties getConfig() {
		return config;
	}

	public void setConfig(ContextProperties config) {
		this.config = config;
		sendActivityReport = "true".equals(config.getProperty("task.sendreport." + name));
		reportRecipients = config.getProperty("task.recipients." + name);
	}

	public void setSender(EMailSender sender) {
		this.sender = sender;
	}

	public void setUserDao(UserDAO userDao) {
		this.userDao = userDao;
	}

	/**
	 * Saves the task configuration
	 * 
	 * @throws ParseException
	 * @throws IOException
	 */
	public void save() throws IOException, ParseException {
		getScheduling().save();
		ContextProperties config = Context.get().getProperties();
		config.setProperty("task.recipients." + name, getReportRecipients());
		config.setProperty("task.sendreport." + name, isSendActivityReport() ? "true" : "false");
		config.write();
	}

	public boolean isSendActivityReport() {
		return sendActivityReport;
	}

	public void setSendActivityReport(boolean sendActivityReport) {
		this.sendActivityReport = sendActivityReport;
	}

	public void setReportRecipients(String reportRecipients) {
		this.reportRecipients = reportRecipients;
	}

	public void setLockManager(LockManager lockManager) {
		this.lockManager = lockManager;
	}

	public void setSystemLoadMonitor(SystemLoadMonitor systemLoadMonitor) {
		this.systemLoadMonitor = systemLoadMonitor;
	}
}