package com.logicaldoc.gui.frontend.client.system.task;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.beans.GUITask;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.data.UsersDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.MultiComboBoxItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel shows notification settings for a task
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.1
 */
public class TaskNotificationPanel extends VLayout {

	private ChangedHandler changedHandler;

	private GUITask task;

	private MultiComboBoxItem recipients;

	public TaskNotificationPanel(GUITask task, ChangedHandler changedHandler) {
		setWidth100();
		this.changedHandler = changedHandler;
		this.task = task;
	}

	@Override
	public void onDraw() {
		VLayout notificationsPane = new VLayout();
		setMembers(notificationsPane);

		final DynamicForm notificationsForm = new DynamicForm();
		notificationsForm.setColWidths(1, "*");
		notificationsForm.setMargin(3);

		List<FormItem> items = new ArrayList<>();

		// Enable/Disable notifications
		CheckboxItem sendReport = new CheckboxItem();
		sendReport.setName("sendReport");
		sendReport.setTitle(I18N.message("sendactivityreport"));
		sendReport.setRedrawOnChange(true);
		sendReport.setWidth(50);
		sendReport.setValue(task.isSendActivityReport());
		sendReport.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				task.setSendActivityReport("true".equals(notificationsForm.getValue("sendReport").toString()));

				// Notify the external handler
				changedHandler.onChanged(event);
			}
		});

		items.add(sendReport);

		Long[] ids = new Long[task.getReportRecipients().length];
		for (int i = 0; i < ids.length; i++)
			ids[i] = task.getReportRecipients()[i].getId();

		recipients = ItemFactory.newMultiComboBoxItem("recipients", "recipients", new UsersDS(null, false, false), ids);
		recipients.setValueField("id");
		recipients.setDisplayField("username");
		recipients.addChangedHandler(changedHandler);
		items.add(recipients);

		notificationsForm.setItems(items.toArray(new FormItem[0]));
		notificationsPane.setMembers(notificationsForm);
	}

	boolean validate() {
		try {
			if (recipients != null) {
				String[] ids = recipients.getValues();
				GUIUser[] recipients = new GUIUser[ids != null ? ids.length : 0];

				if (ids != null && ids.length > 0)
					for (int i = 0; i < ids.length; i++) {
						GUIUser user = new GUIUser();
						user.setId(Long.parseLong(ids[i]));
						recipients[i] = user;
					}

				task.setReportRecipients(recipients);
			}
			return true;
		} catch (Throwable t) {
			return false;
		}
	}
}