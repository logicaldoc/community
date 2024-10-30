package com.logicaldoc.gui.frontend.client.settings;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.EventSelectorOptions;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;

/**
 * Shows the quota details.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.3
 */
public class AuditingPanel extends AdminPanel {

	private static final String WEBSERVICE_CALL_TTL = "webservice.call.ttl";

	private static final String DAYS = "days";

	private static final String HISTORY_DOCUMENT_TTL = "history.document.ttl";

	private static final String HISTORY_FOLDER_TTL = "history.folder.ttl";

	private static final String HISTORY_USER_TTL = "history.user.ttl";

	private static final String HISTORY_IMPORTFOLDER_TTL = "history.importfolder.ttl";

	private static final String HISTORY_WORKFLOW_TTL = "history.workflow.ttl";

	private static final String HISTORY_OCR_TTL = "history.ocr.ttl";

	public AuditingPanel() {
		super("auditing");

		SettingService.Instance.get().loadAuditingSettings(new AsyncCallback<List<GUIParameter>>() {
			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(List<GUIParameter> result) {
				initGUI(result);
			}
		});
	}

	private void initGUI(List<GUIParameter> parameters) {
		ValuesManager vm = new ValuesManager();
		DynamicForm form = new DynamicForm();
		form.setValuesManager(vm);
		form.setTitleOrientation(TitleOrientation.LEFT);
		form.setPadding(5);
		form.setWidth(1);
		form.setNumCols(2);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setWrapItemTitles(false);

		String eventsParameterName = Session.get().getTenantName() + ".history.events";
		final SelectItem eventsSelector = ItemFactory.newEventsSelector(eventsParameterName,
				I18N.message("recordedevents"), null,
				new EventSelectorOptions(true, true, true, true, true, true, true));
		eventsSelector.setColSpan(2);
		eventsSelector.setEndRow(true);

		SpinnerItem documentsTTL = ItemFactory.newSpinnerItem(HISTORY_DOCUMENT_TTL, "ttlhistdocs",
				Integer.parseInt(Util.getParameterValue(parameters, HISTORY_DOCUMENT_TTL)));
		documentsTTL.setRequired(true);
		documentsTTL.setHint(I18N.message(DAYS));
		documentsTTL.setMin(-1);
		documentsTTL.setStep(1);
		documentsTTL.setWidth(80);

		SpinnerItem foldersTTL = ItemFactory.newSpinnerItem(HISTORY_FOLDER_TTL, "ttlhistfolders",
				Integer.parseInt(Util.getParameterValue(parameters, HISTORY_FOLDER_TTL)));
		foldersTTL.setRequired(true);
		foldersTTL.setHint(I18N.message(DAYS));
		foldersTTL.setMin(-1);
		foldersTTL.setStep(1);
		foldersTTL.setWidth(80);

		SpinnerItem usersTTL = ItemFactory.newSpinnerItem(HISTORY_USER_TTL, "ttlhistusers",
				Integer.parseInt(Util.getParameterValue(parameters, HISTORY_USER_TTL)));
		usersTTL.setRequired(true);
		usersTTL.setHint(I18N.message(DAYS));
		usersTTL.setMin(-1);
		usersTTL.setStep(1);
		usersTTL.setWidth(80);

		SpinnerItem importFoldersTTL = ItemFactory.newSpinnerItem(HISTORY_IMPORTFOLDER_TTL, "ttlhistimportfolders",
				Integer.parseInt(Util.getParameterValue(parameters, HISTORY_IMPORTFOLDER_TTL)));
		importFoldersTTL.setRequired(true);
		importFoldersTTL.setHint(I18N.message(DAYS));
		importFoldersTTL.setMin(-1);
		importFoldersTTL.setStep(1);
		importFoldersTTL.setWidth(80);
		importFoldersTTL.setVisible(
				Feature.enabled(Feature.IMPORT_LOCAL_FOLDERS) || Feature.enabled(Feature.IMPORT_REMOTE_FOLDERS));

		SpinnerItem workflowTTL = ItemFactory.newSpinnerItem(HISTORY_WORKFLOW_TTL, "ttlhistworkflows",
				Integer.parseInt(Util.getParameterValue(parameters, HISTORY_WORKFLOW_TTL)));
		workflowTTL.setRequired(true);
		workflowTTL.setHint(I18N.message(DAYS));
		workflowTTL.setMin(-1);
		workflowTTL.setStep(1);
		workflowTTL.setWidth(80);
		workflowTTL.setVisible(Feature.enabled(Feature.WORKFLOW));

		SpinnerItem ocrTTL = ItemFactory.newSpinnerItem(HISTORY_WORKFLOW_TTL, "ttlhistocr",
				Integer.parseInt(Util.getParameterValue(parameters, HISTORY_WORKFLOW_TTL)));
		ocrTTL.setRequired(true);
		ocrTTL.setHint(I18N.message(DAYS));
		ocrTTL.setMin(-1);
		ocrTTL.setStep(1);
		ocrTTL.setWidth(80);
		ocrTTL.setVisible(Feature.enabled(Feature.OCR));

		SpinnerItem webserviceTTL = ItemFactory.newSpinnerItem(WEBSERVICE_CALL_TTL, "ttlhistwebservice",
				Integer.parseInt(Util.getParameterValue(parameters, WEBSERVICE_CALL_TTL)));
		webserviceTTL.setRequired(true);
		webserviceTTL.setHint(I18N.message(DAYS));
		webserviceTTL.setMin(-1);
		webserviceTTL.setStep(1);
		webserviceTTL.setWidth(80);
		webserviceTTL.setVisible(Feature.enabled(Feature.OCR));

		String eventsSetting = Util.getValue(eventsParameterName, parameters);
		if (eventsSetting != null && !eventsSetting.isEmpty()) {
			List<String> selectedEvents = new ArrayList<>();
			for (String token : eventsSetting.split("\\,")) {
				if (!token.isEmpty())
					selectedEvents.add(token);
			}
			eventsSelector.setValue(selectedEvents.toArray(new String[0]));
		}

		if (Session.get().isDefaultTenant())
			form.setItems(documentsTTL, foldersTTL, usersTTL, importFoldersTTL, workflowTTL, ocrTTL, webserviceTTL,
					eventsSelector);
		else
			form.setItems(eventsSelector);

		IButton save = new IButton(I18N.message("save"));
		save.setMinWidth(80);
		save.addClickHandler(event -> onSave(vm));

		body.setMembers(form);
		addMember(save);
	}

	private void onSave(ValuesManager vm) {
		if (Boolean.TRUE.equals(vm.validate())) {
			List<GUIParameter> settings = new ArrayList<>();

			String eventsParameterName = Session.get().getTenantName() + ".history.events";
			final String eventsValue = vm.getValueAsString(eventsParameterName);
			settings.add(new GUIParameter(eventsParameterName,
					eventsValue != null && eventsValue.contains("all") ? "all" : eventsValue));
			settings.add(new GUIParameter(HISTORY_DOCUMENT_TTL, vm.getValueAsString(HISTORY_DOCUMENT_TTL)));
			settings.add(new GUIParameter(HISTORY_FOLDER_TTL, vm.getValueAsString(HISTORY_FOLDER_TTL)));
			settings.add(new GUIParameter(HISTORY_USER_TTL, vm.getValueAsString(HISTORY_USER_TTL)));
			settings.add(new GUIParameter(HISTORY_IMPORTFOLDER_TTL, vm.getValueAsString(HISTORY_IMPORTFOLDER_TTL)));
			settings.add(new GUIParameter(HISTORY_WORKFLOW_TTL, vm.getValueAsString(HISTORY_WORKFLOW_TTL)));
			settings.add(new GUIParameter(HISTORY_OCR_TTL, vm.getValueAsString(HISTORY_OCR_TTL)));
			settings.add(new GUIParameter(WEBSERVICE_CALL_TTL, vm.getValueAsString(WEBSERVICE_CALL_TTL)));

			SettingService.Instance.get().saveSettings(settings, new AsyncCallback<Void>() {

				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(Void ret) {
					GuiLog.info(I18N.message("settingssaved"), null);
				}
			});
		}
	}
}