package com.logicaldoc.gui.frontend.client.settings;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.OCRService;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;

/**
 * This panel shows the OCR settings.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class OCRSettingsPanel extends AdminPanel {

	private static final String FALSE = "false";

	private static final String TRUE = "true";

	private static final String YES = "yes";

	// Stores the engine's parameters
	private DynamicForm engineForm = null;

	private ValuesManager vm = new ValuesManager();

	public OCRSettingsPanel() {
		super("ocr");

		OCRService.Instance.get().loadSettings(new AsyncCallback<GUIParameter[]>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIParameter[] params) {
				initGUI(params);
			}
		});
	}

	private void initGUI(final GUIParameter[] params) {
		DynamicForm form = new DynamicForm();
		form.setValuesManager(vm);
		form.setTitleOrientation(TitleOrientation.LEFT);
		form.setNumCols(2);
		form.setColWidths(1, "*");
		form.setPadding(5);

		// OCR Enabled
		RadioGroupItem enabled = ItemFactory.newBooleanSelector("ocr_enabled", "enabled");
		enabled.setRequired(true);
		enabled.setDisabled(!Session.get().isDefaultTenant());

		SpinnerItem resolutionThreshold = ItemFactory.newSpinnerItem("ocr_resolution_threshold",
				I18N.message("resolutionthreshold"), (Integer) null);
		resolutionThreshold.setRequired(true);
		resolutionThreshold.setWrapTitle(false);
		resolutionThreshold.setHint("pixels");
		resolutionThreshold.setMin(1);
		resolutionThreshold.setStep(1);

		SpinnerItem textThreshold = ItemFactory.newSpinnerItem("ocr_text_threshold", I18N.message("textthreshold"),
				(Integer) null);
		textThreshold.setRequired(true);
		textThreshold.setWrapTitle(false);
		textThreshold.setHint("%");
		textThreshold.setMin(1);
		textThreshold.setMax(100);
		textThreshold.setStep(1);

		TextItem includes = ItemFactory.newTextItem("ocr_includes", "include", null);
		TextItem excludes = ItemFactory.newTextItem("ocr_excludes", "exclude", null);

		SpinnerItem timeout = ItemFactory.newSpinnerItem("ocr_timeout", I18N.message("timeout"), (Integer) null);
		timeout.setRequired(true);
		timeout.setWrapTitle(false);
		timeout.setHint(I18N.message("seconds").toLowerCase());
		timeout.setMin(0);
		timeout.setStep(10);

		SpinnerItem batchTimeout = ItemFactory.newSpinnerItem("ocr_timeout_batch", I18N.message("batchtimeout"),
				(Integer) null);
		batchTimeout.setRequired(true);
		batchTimeout.setWrapTitle(false);
		batchTimeout.setHint(I18N.message("seconds").toLowerCase());
		batchTimeout.setMin(0);
		batchTimeout.setStep(10);

		// Deduct the list of available OCR engines
		Map<String, String> engines = new HashMap<String, String>();
		for (GUIParameter param : params)
			if (param.getName().startsWith("ocr.engine."))
				engines.put(param.getValue(), param.getValue());

		final RadioGroupItem engine = ItemFactory.newBooleanSelector("ocr_engine", "engine");
		engine.setRequired(true);
		engine.setValueMap(engines);
		engine.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				initEngineForm(event.getValue().toString(), params);
			}
		});

		SpinnerItem ocrrendres = ItemFactory.newSpinnerItem("ocr_rendres", I18N.message("ocrrendres"), (Integer) null);
		ocrrendres.setRequired(true);
		ocrrendres.setWrapTitle(false);
		ocrrendres.setHint("dpi");
		ocrrendres.setMin(1);
		ocrrendres.setStep(10);

		SpinnerItem batch = ItemFactory.newSpinnerItem("ocr_batch", I18N.message("batch"), (Integer) null);
		batch.setRequired(true);
		batch.setWrapTitle(false);
		batch.setHint("pages");
		batch.setMin(1);
		batch.setStep(1);

		SpinnerItem maxSize = ItemFactory.newSpinnerItem("ocr_maxsize", I18N.message("maxsize"), (Integer) null);
		maxSize.setRequired(true);
		maxSize.setWrapTitle(false);
		maxSize.setHint("MB");
		maxSize.setMin(1);
		maxSize.setStep(5);

		RadioGroupItem cropImage = ItemFactory.newBooleanSelector("ocr_cropimage",
				I18N.message("cropvisiblepartofimage"));
		cropImage.setWrapTitle(false);
		cropImage.setRequired(true);
		cropImage.setDisabled(!Session.get().isDefaultTenant());

		RadioGroupItem errorOnEmpty = ItemFactory.newBooleanSelector("ocr_erroronempty",
				I18N.message("erroronemptyextraction"));
		errorOnEmpty.setWrapTitle(false);
		errorOnEmpty.setRequired(true);

		for (GUIParameter param : params) {
			if (param.getName().endsWith("ocr.enabled"))
				enabled.setValue(param.getValue().equals(TRUE) ? YES : "no");
			else if (param.getName().endsWith("ocr.cropImage"))
				cropImage.setValue(param.getValue().equals(TRUE) ? YES : "no");
			else if (param.getName().endsWith(".ocr.erroronempty"))
				errorOnEmpty.setValue(param.getValue().equals(TRUE) ? YES : "no");
			else if (param.getName().endsWith("ocr.resolution.threshold"))
				resolutionThreshold.setValue(Integer.parseInt(param.getValue()));
			else if (param.getName().endsWith("ocr.text.threshold"))
				textThreshold.setValue(Integer.parseInt(param.getValue()));
			else if (param.getName().endsWith("ocr.timeout"))
				timeout.setValue(Integer.parseInt(param.getValue()));
			else if (param.getName().endsWith("ocr.timeout.batch"))
				batchTimeout.setValue(Integer.parseInt(param.getValue()));
			else if (param.getName().endsWith("ocr.rendres"))
				ocrrendres.setValue(Integer.parseInt(param.getValue()));
			else if (param.getName().endsWith("ocr.batch"))
				batch.setValue(Integer.parseInt(param.getValue()));
			else if (param.getName().endsWith("ocr.maxsize"))
				maxSize.setValue(Integer.parseInt(param.getValue()));
			else if (param.getName().endsWith(".ocr.includes"))
				includes.setValue(param.getValue());
			else if (param.getName().endsWith(".ocr.excludes"))
				excludes.setValue(param.getValue());
			else if (param.getName().endsWith("ocr.engine"))
				engine.setValue(param.getValue());
		}

		List<FormItem> items = new ArrayList<FormItem>();
		items.add(enabled);

		if (Session.get().isDefaultTenant()) {
			items.addAll(Arrays.asList(timeout, includes, excludes, maxSize, textThreshold, resolutionThreshold,
					ocrrendres, cropImage, batch, batchTimeout, errorOnEmpty, engine));
		} else
			items.addAll(Arrays.asList(includes, excludes, textThreshold, resolutionThreshold, errorOnEmpty));
		form.setItems(items.toArray(new FormItem[0]));

		IButton save = new IButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				onSave();
			}
		});

		body.setMembers(form);
		if (Session.get().isDefaultTenant())
			initEngineForm(engine.getValueAsString(), params);
		addMember(save);
	}

	private void initEngineForm(String engine, GUIParameter[] params) {
		if (engineForm != null)
			body.removeMember(engineForm);

		engineForm = new DynamicForm();
		engineForm.setValuesManager(vm);
		engineForm.setTitleOrientation(TitleOrientation.LEFT);
		engineForm.setNumCols(2);
		engineForm.setColWidths(1, "*");
		engineForm.setPadding(5);

		List<FormItem> items = new ArrayList<FormItem>();
		for (GUIParameter p : params) {
			if (p.getName().startsWith("ocr." + engine + ".")) {
				String title = p.getName().substring(p.getName().lastIndexOf('.') + 1);
				TextItem paramItem = ItemFactory.newTextItem(p.getName().replace('.', '_'), title, p.getValue());
				paramItem.setTitle(title);
				paramItem.setWidth(350);
				items.add(paramItem);
			}
		}
		engineForm.setItems(items.toArray(new FormItem[0]));

		body.addMember(engineForm, 1);
	}

	private void onSave() {
		@SuppressWarnings("unchecked")
		Map<String, Object> values = (Map<String, Object>) vm.getValues();

		if (vm.validate()) {
			List<GUIParameter> params = new ArrayList<GUIParameter>();

			params.add(new GUIParameter(Session.get().getTenantName() + ".ocr.includes",
					(String) values.get("ocr_includes")));
			params.add(new GUIParameter(Session.get().getTenantName() + ".ocr.excludes",
					(String) values.get("ocr_excludes")));
			
			if (values.get("ocr_text_threshold") instanceof Integer)
				params.add(new GUIParameter(Session.get().getTenantName() + ".ocr.text.threshold",
						((Integer) values.get("ocr_text_threshold")).toString()));
			else
				params.add(new GUIParameter(Session.get().getTenantName() + ".ocr.text.threshold",
						(String) values.get("ocr_text_threshold")));

			if (values.get("ocr_resolution_threshold") instanceof Integer)
				params.add(new GUIParameter(Session.get().getTenantName() + ".ocr.resolution.threshold",
						((Integer) values.get("ocr_resolution_threshold")).toString()));
			else
				params.add(new GUIParameter(Session.get().getTenantName() + ".ocr.resolution.threshold",
						(String) values.get("ocr_resolution_threshold")));
			
			params.add(new GUIParameter(Session.get().getTenantName() + ".ocr.erroronempty",
					values.get("ocr_erroronempty").equals(YES) ? TRUE : FALSE));

			if (Session.get().isDefaultTenant()) {
				params.add(new GUIParameter("ocr.enabled", values.get("ocr_enabled").equals(YES) ? TRUE : FALSE));

				params.add(new GUIParameter("ocr.cropImage",
						values.get("ocr_cropimage").equals(YES) ? TRUE : FALSE));
				
				if (values.get("ocr_timeout") instanceof Integer)
					params.add(new GUIParameter("ocr.timeout", ((Integer) values.get("ocr_timeout")).toString()));
				else
					params.add(new GUIParameter("ocr.timeout", (String) values.get("ocr_timeout")));

				if (values.get("ocr_timeout_batch") instanceof Integer)
					params.add(new GUIParameter("ocr.timeout.batch",
							((Integer) values.get("ocr_timeout_batch")).toString()));
				else
					params.add(new GUIParameter("ocr.timeout.batch", (String) values.get("ocr_timeout_batch")));

				if (values.get("ocr_maxsize") instanceof Integer)
					params.add(new GUIParameter("ocr.maxsize", ((Integer) values.get("ocr_maxsize")).toString()));
				else
					params.add(new GUIParameter("ocr.maxsize", (String) values.get("ocr_maxsize")));

				if (values.get("ocr_rendres") instanceof Integer)
					params.add(new GUIParameter("ocr.rendres", ((Integer) values.get("ocr_rendres")).toString()));
				else
					params.add(new GUIParameter("ocr.rendres", (String) values.get("ocr_rendres")));

				if (values.get("ocr_batch") instanceof Integer)
					params.add(new GUIParameter("ocr.batch", ((Integer) values.get("ocr_batch")).toString()));
				else
					params.add(new GUIParameter("ocr.batch", (String) values.get("ocr_batch")));

				String engine = (String) values.get("ocr_engine");
				params.add(new GUIParameter("ocr.engine", engine));
				
				for (String name : values.keySet()) {
					if (name.startsWith("ocr_" + engine + "_"))
						params.add(new GUIParameter(name.replace('_', '.'), values.get(name).toString()));
				}
			}
			
			SettingService.Instance.get().saveSettings(params.toArray(new GUIParameter[0]), new AsyncCallback<Void>() {

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