package com.logicaldoc.gui.frontend.client.folder;

import java.util.Map;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.UsersDS;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.MultiComboBoxItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;

/**
 * Shows the folder's quotas
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.4.3
 */
public class FolderQuotaPanel extends FolderDetailTab {

	private DynamicForm form = new DynamicForm();

	private ValuesManager vm = new ValuesManager();

	private boolean update = false;

	private MultiComboBoxItem recipients;

	public FolderQuotaPanel(GUIFolder folder, ChangedHandler changedHandler) {
		super(folder, changedHandler);
		setWidth100();
		setHeight100();
		setMembersMargin(20);
		update = folder.isWorkspace() && Session.get().getUser().isMemberOf(Constants.GROUP_ADMIN);
		refresh();
	}

	private void refresh() {
		vm = new ValuesManager();

		if (form != null)
			form.destroy();

		if (contains(form))
			removeChild(form);

		form = new DynamicForm();
		form.setValuesManager(vm);
		form.setWrapItemTitles(false);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(2);

		TextItem documentsQuota = ItemFactory.newLongItem("documentsquota", "documentsquota", folder.getQuotaDocs());
		documentsQuota.setDisabled(!update);
		if (update)
			documentsQuota.addChangedHandler(changedHandler);

		TextItem sizeQuota = ItemFactory.newLongItem("sizequota", "sizequota", folder.getQuotaSize());
		sizeQuota.setHint("MB");
		sizeQuota.setWidth(120);
		sizeQuota.setDisabled(!update);
		if (update)
			sizeQuota.addChangedHandler(changedHandler);

		StaticTextItem size = ItemFactory.newStaticTextItem("ssize", "size", Util.formatSizeW7(folder.getSizeTotal()));
		size.setWrap(false);

		StaticTextItem documents = ItemFactory.newStaticTextItem("documents",
				Util.formatLong(folder.getDocumentsTotal()));

		SpinnerItem quotaThreshold = ItemFactory.newSpinnerItem("quotaThreshold", "alertthreshold",
				folder.getQuotaThreshold());
		quotaThreshold.setDisabled(!update);
		quotaThreshold.setMax(100);
		quotaThreshold.setMin(0);
		quotaThreshold.setHint("%");
		if (update)
			quotaThreshold.addChangedHandler(changedHandler);

		recipients = ItemFactory.newMultiComboBoxItem("recipients", "alertrecipients", new UsersDS(null, false, false),
				folder.getQuotaAlertRecipients());
		recipients.setValueField("username");
		recipients.setDisplayField("username");
		if (update)
			recipients.addChangedHandler(changedHandler);

		documentsQuota.setDisabled(!update);
		sizeQuota.setDisabled(!update);
		quotaThreshold.setDisabled(!update);
		recipients.setDisabled(!update);

		form.setItems(documentsQuota, documents, sizeQuota, size, quotaThreshold, recipients);
		addMember(form);
	}

	@Override
	public boolean validate() {
		@SuppressWarnings("unchecked")
		Map<String, Object> values = (Map<String, Object>) vm.getValues();

		vm.validate();
		if (vm.hasErrors())
			return false;

		if (values.get("documentsquota") == null)
			folder.setQuotaDocs(null);
		else
			folder.setQuotaDocs(Long.parseLong(values.get("documentsquota").toString()));

		if (values.get("sizequota") == null)
			folder.setQuotaSize(null);
		else
			folder.setQuotaSize(Long.parseLong(values.get("sizequota").toString()));

		if (values.get("quotaThreshold") == null)
			folder.setQuotaThreshold(null);
		else
			folder.setQuotaThreshold(Integer.parseInt(values.get("quotaThreshold").toString()));

		folder.clearQuotaAlertRecipients();
		String[] usernames = recipients.getValues();
		if (usernames != null && usernames.length > 0)
			for (int i = 0; i < usernames.length; i++)
				folder.addQuotaAlertRecipient(usernames[i]);

		return !vm.hasErrors();
	}
}