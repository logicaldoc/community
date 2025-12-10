package com.logicaldoc.web.util;

import java.util.Date;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.CancellationException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.communication.Recipient;
import com.logicaldoc.core.communication.SystemMessage;
import com.logicaldoc.core.communication.SystemMessageDAO;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.core.threading.NotifyingThread;
import com.logicaldoc.core.threading.NotifyingThread.ThreadCompleteListener;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.time.TimeDiff;

/**
 * A listener of {@link NotifyingThread} designed to handle the completion of a
 * long running operation. The purpose is to alert a set of users about the
 * completion using a system message
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.3
 * 
 * @param <T> type of listener
 */
public class LongRunningOperationCompleteListener<T> implements ThreadCompleteListener<T> {

	private static final Logger log = LoggerFactory.getLogger(LongRunningOperationCompleteListener.class);

	private Set<String> usernames = new HashSet<>();

	/**
	 * Creates a new listener with just one user to notify
	 * 
	 * @param username the user to alert
	 */
	public LongRunningOperationCompleteListener(String username) {
		super();
		usernames.add(username);
	}

	/**
	 * Creates a new listener
	 * 
	 * @param usernames the user names to alert
	 */
	public LongRunningOperationCompleteListener(Set<String> usernames) {
		super();
		this.usernames = usernames;
	}

	@Override
	public void completed(NotifyingThread<T> thread) {
		UserDAO uDao = UserDAO.get();
		Date now = new Date();

		for (String username : usernames) {
			User user;
			try {
				user = uDao.findByUsername(username);
				if (user == null)
					throw new PersistenceException("Unexisting user %s".formatted(username));
			} catch (PersistenceException e) {
				log.warn("Error retrieving user {}", username);
				continue;
			}

			Recipient recipient = new Recipient();
			recipient.setName(user.getUsername());
			recipient.setAddress(user.getUsername());
			recipient.setType(Recipient.TYPE_SYSTEM);
			recipient.setMode("message");
			Set<Recipient> recipients = new HashSet<>();
			recipients.add(recipient);
			SystemMessage sysmess = new SystemMessage();
			sysmess.setAuthor("SYSTEM");
			sysmess.setRecipients(recipients);
			sysmess.setSubject(I18N.message("operationcompleted2", user.getLocale(), thread.getName()));
			if (thread.getError() == null) {
				sysmess.setMessageText(I18N.message("longrunningoperationcompleted", user.getLocale(),
						new Object[] { thread.getName(), TimeDiff.printDuration(thread.getElapsedTime()) }));
				sysmess.setPrio(0);
			} else {
				String errorMessage = thread.getError().getMessage();
				if (thread.getError() instanceof InterruptedException
						|| thread.getError() instanceof CancellationException
						|| thread.getError().getCause() instanceof InterruptedException)
					errorMessage = I18N.message("interrupted", user.getLocale());

				sysmess.setMessageText(
						I18N.message("longrunningoperationcompletedwitherror", user.getLocale(), new Object[] {
								thread.getName(), TimeDiff.printDuration(thread.getElapsedTime()), errorMessage }));
				sysmess.setPrio(2);
				if (log.isErrorEnabled())
					log.error("Thread {} ended in error after {}", thread.getName(),
							TimeDiff.printDuration(thread.getElapsedTime()), thread.getError());
			}

			sysmess.setSentDate(now);
			sysmess.setConfirmation(0);
			sysmess.setDateScope(1);

			saveMessage(sysmess);
		}
	}

	private void saveMessage(SystemMessage message) {
		try {
			SystemMessageDAO.get().store(message);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
	}
}