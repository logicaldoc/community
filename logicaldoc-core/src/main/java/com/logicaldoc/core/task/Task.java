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
import org.apache.commons.lang3.time.StopWatch;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.automation.Automation;
import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailSender;
import com.logicaldoc.core.communication.Recipient;
import com.logicaldoc.core.lock.LockManager;
import com.logicaldoc.core.runtime.Aspect;
import com.logicaldoc.core.runtime.RunLevel;
import com.logicaldoc.core.searchengine.IndexException;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.core.system.SystemLoadMonitor;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.spring.Context;
import com.logicaldoc.util.time.TimeDiff;

import jakarta.annotation.PostConstruct;
import jakarta.annotation.Resource;

/**
 * A task is a long running process that is fired regularly
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 3.5.0
 */
public abstract class Task implements Runnable {

	protected Logger log = LoggerFactory.getLogger(Task.class);

	public static final int STATUS_IDLE = 0;

	public static final int STATUS_RUNNING = 1;

	public static final int STATUS_STOPPING = 2;

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

	@Resource(name = "ContextProperties")
	protected ContextProperties config;

	@Resource(name = "EMailSender")
	protected EMailSender sender = null;

	@Resource(name = "userDAO")
	protected UserDAO userDao = null;

	@Resource(name = "lockManager")
	protected LockManager lockManager;

	@Resource(name = "systemLoadMonitor")
	protected SystemLoadMonitor systemLoadMonitor;

	protected boolean sendActivityReport = false;

	private String reportRecipients = null;

	protected String transactionId = null;

	private Random random = new Random();

	protected Task(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	protected void setName(String name) {
		this.name = name;
	}

	private synchronized void setStatus(int status) {
		if (status != STATUS_IDLE && status != STATUS_RUNNING && status != STATUS_STOPPING)
			throw new InvalidParameterException("Invalid status value");
		boolean needNotification = this.status != status;
		this.status = status;
		if (needNotification)
			for (TaskListener listener : taskListeners)
				listener.statusChanged(status);

	}

	/**
	 * Increments the progress by one and detects system overload.
	 */
	protected synchronized void next() {
		setProgress(progress + 1);

		// Get the lock
		getLock();

		if (systemLoadMonitor != null) {
			boolean overload = false;

			while (systemLoadMonitor.isAverageCpuOverLoaded() && !interruptRequested) {
				if (!overload) {
					overload = true;
					log.info("Execution paused because of system overload");
				}

				// Always get the lock to maintain it
				getLock();

				synchronized (this) {
					try {
						wait((1 + random.nextInt(20)) * 1000L);
					} catch (InterruptedException e) {
						Thread.currentThread().interrupt();
					}
				}

				checkExpiration();
			}

			if (overload)
				log.info("Execution resumed after system overload");
		}
	}

	private void getLock() {
		if (lockManager != null)
			lockManager.get(getName(), transactionId);
	}

	protected synchronized void setProgress(long progress) {
		try {
			if (progress > getSize() || progress < 0)
				return;

			boolean needNotification = this.progress != progress;
			this.progress = progress;
			if (needNotification)
				for (TaskListener listener : taskListeners)
					listener.progressChanged(progress);
		} catch (Exception t) {
			// Nothing to do
		} finally {
			checkExpiration();
		}
	}

	/**
	 * Checks if the time has been expired and in that case interrupts the
	 * elaboration.
	 */
	private void checkExpiration() {
		// Check if time was expired, and request interruption if the case
		if (getScheduling().isExpired()) {
			log.warn("The timeout has been reached, the elaboration is being interrupted");
			interrupt();
		}
	}

	@Override
	public void run() {
		if (!RunLevel.current().aspectEnabled(Aspect.SCHEDULEDTASKS)) {
			log.debug("Aspect scheduledTasks not enabled");
			return;
		}

		if (!getScheduling().isEnabled()) {
			log.debug("Task {} is disabled", getName());
			return;
		}

		if (getStatus() != STATUS_IDLE) {
			log.debug("Task {} is already running", getName());
			return;
		}

		log.info("Task {} started", getName());
		interruptRequested = false;
		setStatus(STATUS_RUNNING);
		getScheduling().setPreviousFireTime(new Date());
		setProgress(0);
		lastRunError = null;

		StopWatch stopWatch = new StopWatch();
		try {
			/*
			 * Need to acquire the lock
			 */
			transactionId = UUID.randomUUID().toString();
			if (isConcurrent() || (lockManager != null && lockManager.get(getName(), transactionId))) {
				stopWatch.start();
				runTask();
				getScheduling().setLastDuration(stopWatch.getTime());
			}
		} catch (InterruptedException ie) {
			log.error("The task gets interrupted");
			Thread.currentThread().interrupt();
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			log.error("The task is stopped");
			lastRunError = e;
		} finally {
			stopWatch.stop();

			// In any case release the lock
			try {
				if (lockManager != null)
					lockManager.release(getName(), transactionId);
			} catch (Exception t) {
				// Nothing to do
			}

			setStatus(STATUS_IDLE);
			interruptRequested = false;
			saveWork();
			getScheduling().setLastDuration(stopWatch.getTime());
			log.info("Task {} completed in {}", getName(), TimeDiff.printDuration(getScheduling().getLastDuration()));
			if (isSendActivityReport() && StringUtils.isNotEmpty(getReportRecipients()))
				notifyReport();
			transactionId = null;
		}
	}

	public synchronized void interrupt() {
		if (isRunning()) {
			interruptRequested = true;
			setStatus(STATUS_STOPPING);
		}
	}

	public synchronized boolean isInterrupted() {
		return getStatus() == STATUS_IDLE;
	}

	/**
	 * The the total size of the processing(number of units of work)
	 * 
	 * @return total number of steps
	 */
	public long getSize() {
		return size;
	}

	public void setSize(long size) {
		this.size = size;
	}

	/**
	 * The task status.
	 * 
	 * @see #STATUS_IDLE
	 * @see #STATUS_RUNNING
	 * @see #STATUS_STOPPING
	 * 
	 * @return the status
	 */
	public synchronized int getStatus() {
		return status;
	}

	/**
	 * The current processing step
	 * 
	 * @return the current step
	 */
	public synchronized long getProgress() {
		return progress;
	}

	/**
	 * The percentage of completion(1-100)
	 * 
	 * @return a percentage(1-100)
	 */
	public int getCompletionPercentage() {
		if (isIndeterminate()) {
			if (getStatus() == STATUS_IDLE)
				return 0;
			else
				return 1;
		} else {
			if (getSize() == 0)
				return 0;
			return (int) (Math.round(((double) progress / (double) getSize()) * 100));
		}
	}

	/**
	 * Check if the task is currently running
	 *
	 * @return true if the task is running({@link #STATUS_RUNNING})
	 */
	public synchronized boolean isRunning() {
		return getStatus() == STATUS_RUNNING;
	}

	/**
	 * Concrete implementations must insert here the code needed to save the
	 * elaboration state in a persistent store
	 */
	public void saveWork() {
		// By default do nothing
	}

	/**
	 * Scheduling policies
	 * 
	 * @return the scheduling policies
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
			User recipient = null;
			try {
				recipient = userDao.findById(Long.parseLong(userId));
			} catch (NumberFormatException e1) {
				// Nothing to do
			} catch (PersistenceException e1) {
				log.error(e1.getMessage(), e1);
			}
			if (recipient == null || StringUtils.isEmpty(recipient.getEmail()))
				continue;

			EMail email = new EMail();
			email.setHtml(true);
			email.setLocale(recipient.getLocale());

			// Prepare the mail recipient
			Set<Recipient> rec = new HashSet<>();
			Recipient r = new Recipient();
			r.setAddress(recipient.getEmail());
			r.setType(Recipient.TYPE_EMAIL);
			r.setMode(Recipient.MODE_EMAIL_TO);
			rec.add(r);
			email.setRecipients(rec);

			// Prepare the arguments for the template
			Map<String, Object> dictionary = new HashMap<>();
			dictionary.put(Automation.LOCALE, recipient.getLocale());
			dictionary.put("task", I18N.message("task.name." + name, recipient.getLocale()));
			dictionary.put("started", scheduling.getPreviousFireTime());
			dictionary.put("ended", new Date());
			dictionary.put("duration", getScheduling().getLastDuration());
			dictionary.put("error", (lastRunError != null ? lastRunError.getMessage() : null));
			dictionary.put("report",
					StringUtils.defaultString(prepareReport(recipient.getLocale())).replace("\\n", "<br />"));

			// Send the email...
			try {
				sender.send(email, "task.report", dictionary);
				log.info("Report sent to: {}", recipient.getEmail());
			} catch (Exception e) {
				log.error(e.getMessage(), e);
			}
		}
	}

	/**
	 * Implementations may compose a locale specific report.
	 * 
	 * @param locale the locale to use for the report
	 * 
	 * @return the report's body
	 */
	protected String prepareReport(Locale locale) {
		return "";
	}

	/**
	 * Concrete implementations must override this method implementing their own
	 * processing logic.
	 * 
	 * @throws IndexException If something goes wrong this exception is raised
	 */
	protected abstract void runTask() throws TaskException, InterruptedException;

	/**
	 * Concrete implementations must override this method declaring if the task
	 * is indeterminate. An indeterminate task is not able to compute it's time
	 * length
	 * 
	 * @return true if the task is indeterminate
	 */
	public abstract boolean isIndeterminate();

	/**
	 * Concrete implementations must override this method declaring if the task
	 * supports multiple instances running concurrently. tHIbernat
	 * 
	 * @return true if the task is concurrent
	 */
	public abstract boolean isConcurrent();

	public String getReportRecipients() {
		return reportRecipients;
	}

	public ContextProperties getConfig() {
		return config;
	}

	/**
	 * Saves the task configuration
	 * 
	 * @throws ParseException raised if the configuration cannot be parsed
	 * @throws IOException raised if the configuration cannot be written
	 */
	public void save() throws IOException, ParseException {
		getScheduling().save();
		ContextProperties props = Context.get().getProperties();
		props.setProperty("task.recipients." + name, getReportRecipients());
		props.setProperty("task.sendreport." + name, isSendActivityReport() ? "true" : "false");
		props.write();
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

	public boolean isInterruptRequested() {
		return interruptRequested;
	}

	@PostConstruct
	protected void init() {
		sendActivityReport = config.getBoolean("task.sendreport." + name);
		reportRecipients = config.getProperty("task.recipients." + name);
	}
}