package com.logicaldoc.gui.frontend.client.settings;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
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
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.FloatItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.ToggleItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.validator.FloatRangeValidator;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * This panel shows the OCR settings.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class OCRSettingsPanel extends AdminPanel {

	private static final String OCR_EVENTS_RECORD = "ocr_events_record";

	private static final String OCR_EVENTS_MAXTEXT = "ocr_events_maxtext";

	private static final String OCR_TIMEOUT_BATCH = "ocr_timeout_batch";

	private static final String OCR_RENDRES = "ocr_rendres";

	private static final String OCR_DOT_THREADS = "ocr.threads";

	private static final String OCR_RESOLUTION_THRESHOLD = "ocr_resolution_threshold";

	private static final String OCR_TEXT_THRESHOLD = "ocr_text_threshold";

	private static final String OCR_DOT_TIMEOUT = "ocr.timeout";

	private static final String OCR_TIMEOUT = "ocr_timeout";

	private static final String SECONDS = "seconds";

	private static final String OCR_THREADS_WAIT = "ocr_threads_wait";

	private static final String OCR_THREADS = "ocr_threads";

	private static final String OCR_MAXSIZE = "ocr_maxsize";

	private static final String OCR_BATCH = "ocr_batch";

	// Stores the engine's parameters
	private DynamicForm engineForm = null;

	private ValuesManager vm = new ValuesManager();

	public OCRSettingsPanel() {
		super("ocr");

		OCRService.Instance.get().loadSettings(new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(List<GUIParameter> params) {
				initGUI(params);
			}
		});
	}

	private void initGUI(final List<GUIParameter> params) {
		DynamicForm form = new DynamicForm();
		form.setValuesManager(vm);
		form.setTitleOrientation(TitleOrientation.LEFT);
		form.setNumCols(4);
		form.setPadding(5);
		form.setWidth(1);

		// OCR Enabled
		ToggleItem enabled = prepareEnabledSwitch(params);

		SpinnerItem resolutionThreshold = prepareResolutionThresholdSpinner(params);

		FloatItem textThreshold = prepareTextThresholdSpinner(params);

		TextItem includes = ItemFactory.newTextItem("ocr_includes", "include",
				com.logicaldoc.gui.common.client.util.Util.getParameterValue(params, "ocr.includes"));
		includes.setWidth(300);
		includes.setColSpan(4);

		TextItem excludes = ItemFactory.newTextItem("ocr_excludes", "exclude",
				com.logicaldoc.gui.common.client.util.Util.getParameterValue(params, "ocr.excludes"));
		excludes.setWidth(300);
		excludes.setColSpan(4);

		SpinnerItem timeout = prepareTimeoutSpinner(params);

		SpinnerItem batchTimeout = prepareBatchTimeoutSpinner(params);

		final RadioGroupItem engine = prepareEngineOptions(params);

		SpinnerItem ocrrendres = prepareOcrRendResSpinner(params);

		SpinnerItem batch = ItemFactory.newSpinnerItem(OCR_BATCH, I18N.message("batch"), Integer.parseInt(
				com.logicaldoc.gui.common.client.util.Util.getParameterValue(params, OCR_BATCH.replace('_', '.'))));
		batch.setRequired(true);
		batch.setEndRow(true);
		batch.setWrapTitle(false);
		batch.setHint("pages");
		batch.setMin(1);
		batch.setStep(1);

		SpinnerItem maxSize = ItemFactory.newSpinnerItem(OCR_MAXSIZE, I18N.message("maxsize"), Integer.parseInt(
				com.logicaldoc.gui.common.client.util.Util.getParameterValue(params, OCR_MAXSIZE.replace('_', '.'))));
		maxSize.setRequired(true);
		maxSize.setEndRow(true);
		maxSize.setWrapTitle(false);
		maxSize.setHint("MB");
		maxSize.setMin(1);
		maxSize.setStep(5);

		SpinnerItem threads = ItemFactory.newSpinnerItem(OCR_THREADS, I18N.message("allowedthreads"), Integer.parseInt(
				com.logicaldoc.gui.common.client.util.Util.getParameterValue(params, OCR_THREADS.replace('_', '.'))));
		threads.setRequired(true);
		threads.setEndRow(true);
		threads.setWrapTitle(false);
		threads.setMin(1);
		threads.setStep(1);

		SpinnerItem threadsWait = ItemFactory.newSpinnerItem(OCR_THREADS_WAIT, I18N.message("waitforthread"),
				Integer.parseInt(com.logicaldoc.gui.common.client.util.Util.getParameterValue(params,
						OCR_THREADS_WAIT.replace('_', '.'))));
		threadsWait.setRequired(true);
		threadsWait.setEndRow(true);
		threadsWait.setWrapTitle(false);
		threadsWait.setHint(I18N.message(SECONDS).toLowerCase());
		threadsWait.setMin(1);
		threadsWait.setStep(10);

		ToggleItem cropImage = prepareCropImageSwitch(params);

		ToggleItem errorOnEmpty = ItemFactory.newToggleItem("ocr_erroronempty", I18N.message("erroronemptyextraction"),
				com.logicaldoc.gui.common.client.util.Util.getParameterValueAsBoolean(params, "ocr.erroronempty"));
		errorOnEmpty.setWrapTitle(false);
		errorOnEmpty.setRequired(true);
		errorOnEmpty.setEndRow(true);

		ToggleItem recordEvents = ItemFactory.newToggleItem(OCR_EVENTS_RECORD, I18N.message("recordevents"),
				com.logicaldoc.gui.common.client.util.Util.getParameterValueAsBoolean(params, "ocr.events.record"));
		recordEvents.setWrapTitle(false);
		recordEvents.setRequired(true);

		SpinnerItem eventMaxText = ItemFactory.newSpinnerItem(OCR_EVENTS_MAXTEXT, I18N.message("nomorethan"),
				Integer.parseInt(com.logicaldoc.gui.common.client.util.Util.getParameterValue(params,
						OCR_EVENTS_MAXTEXT.replace('_', '.'))));
		eventMaxText.setRequired(true);
		eventMaxText.setWrapTitle(false);
		eventMaxText.setHint(I18N.message("characters").toLowerCase());
		eventMaxText.setMin(0);
		eventMaxText.setStep(10);

		List<FormItem> items = new ArrayList<>();
		items.add(enabled);

		if (Session.get().isDefaultTenant()) {
			items.addAll(Arrays.asList(timeout, includes, excludes, maxSize, textThreshold, resolutionThreshold,
					ocrrendres, cropImage, batch, batchTimeout, threads, threadsWait, errorOnEmpty, recordEvents,
					eventMaxText, engine));
		} else
			items.addAll(Arrays.asList(includes, excludes, textThreshold, resolutionThreshold, errorOnEmpty));
		form.setItems(items.toArray(new FormItem[0]));

		IButton save = new IButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(click -> save());

		body.setMembers(form);
		if (Session.get().isDefaultTenant())
			initEngineForm(engine.getValueAsString(), params);
		addMember(save);

		Tab history = new Tab();
		history.setTitle(I18N.message("history"));
		history.setPane(new OCRHistoryPanel());
		tabs.addTab(history);
	}

	private ToggleItem prepareCropImageSwitch(List<GUIParameter> params) {
		ToggleItem cropImage = ItemFactory.newToggleItem("ocr_cropimage", I18N.message("cropvisiblepartofimage"),
				com.logicaldoc.gui.common.client.util.Util.getParameterValueAsBoolean(params, "ocr.cropImage"));
		cropImage.setWrapTitle(false);
		cropImage.setRequired(true);
		cropImage.setEndRow(true);
		cropImage.setDisabled(!Session.get().isDefaultTenant());
		return cropImage;
	}

	private SpinnerItem prepareOcrRendResSpinner(List<GUIParameter> params) {
		SpinnerItem ocrrendres = ItemFactory.newSpinnerItem(OCR_RENDRES, I18N.message("ocrrendres"), Integer
				.parseInt(com.logicaldoc.gui.common.client.util.Util.getParameterValue(params, OCR_DOT_TIMEOUT)));
		ocrrendres.setRequired(true);
		ocrrendres.setEndRow(true);
		ocrrendres.setWrapTitle(false);
		ocrrendres.setHint("dpi");
		ocrrendres.setMin(1);
		ocrrendres.setStep(10);
		return ocrrendres;
	}

	private RadioGroupItem prepareEngineOptions(final List<GUIParameter> params) {
		// Deduct the list of available OCR engines
		Map<String, String> engines = new HashMap<>();
		for (GUIParameter param : params)
			if (param.getName().startsWith("ocr.engine."))
				engines.put(param.getValue(), param.getValue());

		final RadioGroupItem engine = ItemFactory.newRadioGroup("ocr_engine", "engine");
		engine.setRequired(true);
		engine.setEndRow(true);
		engine.setValueMap(engines);
		engine.addChangedHandler((ChangedEvent event) -> initEngineForm(event.getValue().toString(), params));
		engine.setValue(com.logicaldoc.gui.common.client.util.Util.getParameterValue(params, "ocr.engine"));
		return engine;
	}

	private SpinnerItem prepareBatchTimeoutSpinner(List<GUIParameter> params) {
		SpinnerItem batchTimeout = ItemFactory.newSpinnerItem(OCR_TIMEOUT_BATCH, I18N.message("batchtimeout"),
				Integer.parseInt(com.logicaldoc.gui.common.client.util.Util.getParameterValue(params,
						OCR_TIMEOUT_BATCH.replace('_', '.'))));
		batchTimeout.setRequired(true);
		batchTimeout.setEndRow(true);
		batchTimeout.setWrapTitle(false);
		batchTimeout.setHint(I18N.message(SECONDS).toLowerCase());
		batchTimeout.setMin(0);
		batchTimeout.setStep(10);
		return batchTimeout;
	}

	private SpinnerItem prepareTimeoutSpinner(List<GUIParameter> params) {
		SpinnerItem timeout = ItemFactory.newSpinnerItem(OCR_TIMEOUT, I18N.message("timeout"), Integer
				.parseInt(com.logicaldoc.gui.common.client.util.Util.getParameterValue(params, OCR_DOT_TIMEOUT)));
		timeout.setEndRow(true);
		timeout.setRequired(true);
		timeout.setWrapTitle(false);
		timeout.setHint(I18N.message(SECONDS).toLowerCase());
		timeout.setMin(0);
		timeout.setStep(10);
		return timeout;
	}

	private FloatItem prepareTextThresholdSpinner(List<GUIParameter> params) {
		FloatItem textThreshold = ItemFactory.newFloatItem(OCR_TEXT_THRESHOLD, I18N.message("textthreshold"),
				Float.parseFloat(com.logicaldoc.gui.common.client.util.Util.getParameterValue(params,
						OCR_TEXT_THRESHOLD.replace('_', '.'))));
		textThreshold.setEndRow(true);
		textThreshold.setRequired(true);
		textThreshold.setWrapTitle(false);
		textThreshold.setHint("%");
		textThreshold.setWidth(60);
		FloatRangeValidator rangeValidator = new FloatRangeValidator();
		rangeValidator.setMin(0);
		rangeValidator.setMax(100);
		rangeValidator.setErrorMessage(I18N.message("percentageerror"));
		textThreshold.setValidators(rangeValidator);
		return textThreshold;
	}

	private SpinnerItem prepareResolutionThresholdSpinner(List<GUIParameter> params) {
		SpinnerItem resolutionThreshold = ItemFactory.newSpinnerItem(OCR_RESOLUTION_THRESHOLD,
				I18N.message("resolutionthreshold"), Integer.parseInt(com.logicaldoc.gui.common.client.util.Util
						.getParameterValue(params, OCR_RESOLUTION_THRESHOLD.replace('_', '.'))));
		resolutionThreshold.setRequired(true);
		resolutionThreshold.setEndRow(true);
		resolutionThreshold.setWrapTitle(false);
		resolutionThreshold.setHint("pixels");
		resolutionThreshold.setMin(1);
		resolutionThreshold.setStep(1);
		return resolutionThreshold;
	}

	private ToggleItem prepareEnabledSwitch(List<GUIParameter> params) {
		ToggleItem enabled = ItemFactory.newToggleItem("ocr_enabled", "enabled",
				com.logicaldoc.gui.common.client.util.Util.getParameterValueAsBoolean(params, "ocr.enabled"));
		enabled.setRequired(true);
		enabled.setEndRow(true);
		enabled.setDisabled(!Session.get().isDefaultTenant());
		return enabled;
	}

	private void initEngineForm(String engine, List<GUIParameter> params) {
		if (engineForm != null)
			body.removeMember(engineForm);

		engineForm = new DynamicForm();
		engineForm.setValuesManager(vm);
		engineForm.setTitleOrientation(TitleOrientation.LEFT);
		engineForm.setNumCols(2);
		engineForm.setColWidths(1, "*");
		engineForm.setPadding(5);

		List<FormItem> items = new ArrayList<>();
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

	private void save() {
		if (Boolean.FALSE.equals(vm.validate()))
			return;

		List<GUIParameter> params = new ArrayList<>();

		params.add(
				new GUIParameter(Session.get().getTenantName() + ".ocr.includes", vm.getValueAsString("ocr_includes")));
		params.add(
				new GUIParameter(Session.get().getTenantName() + ".ocr.excludes", vm.getValueAsString("ocr_excludes")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".ocr.text.threshold",
				vm.getValueAsString(OCR_TEXT_THRESHOLD)));
		params.add(new GUIParameter(Session.get().getTenantName() + ".ocr.resolution.threshold",
				vm.getValueAsString(OCR_RESOLUTION_THRESHOLD)));
		params.add(new GUIParameter(Session.get().getTenantName() + ".ocr.erroronempty",
				vm.getValueAsString("ocr_erroronempty")));

		if (Session.get().isDefaultTenant())
			collectValuesForWholeSystem(params);

		doSaveSettings(params);
	}

	private void collectValuesForWholeSystem(List<GUIParameter> params) {
		params.add(new GUIParameter("ocr.enabled", vm.getValueAsString("ocr_enabled")));
		params.add(new GUIParameter("ocr.cropImage", vm.getValueAsString("ocr_cropimage")));
		params.add(new GUIParameter(OCR_DOT_TIMEOUT, vm.getValueAsString(OCR_TIMEOUT)));
		params.add(new GUIParameter(OCR_TIMEOUT_BATCH.replace('_', '.'), vm.getValueAsString(OCR_TIMEOUT_BATCH)));
		params.add(new GUIParameter(OCR_MAXSIZE.replace('_', '.'), vm.getValueAsString(OCR_MAXSIZE)));
		params.add(new GUIParameter("ocr.rendres", vm.getValueAsString(OCR_RENDRES)));
		params.add(new GUIParameter(OCR_BATCH.replace('_', '.'), vm.getValueAsString(OCR_BATCH)));
		params.add(new GUIParameter(OCR_EVENTS_MAXTEXT.replace('_', '.'), vm.getValueAsString(OCR_EVENTS_MAXTEXT)));
		params.add(new GUIParameter(OCR_EVENTS_RECORD.replace('_', '.'), vm.getValueAsString(OCR_EVENTS_RECORD)));
		params.add(new GUIParameter(OCR_DOT_THREADS, vm.getValueAsString(OCR_THREADS)));
		params.add(new GUIParameter(OCR_THREADS_WAIT.replace('_', '.'), vm.getValueAsString(OCR_THREADS_WAIT)));

		String engine = vm.getValueAsString("ocr_engine");
		params.add(new GUIParameter("ocr.engine", engine));

		for (Object key : vm.getValues().keySet()) {
			String name = key.toString();
			if (name.startsWith("ocr_" + engine + "_"))
				params.add(new GUIParameter(name.replace('_', '.'), vm.getValueAsString(name)));
		}
	}

	private void doSaveSettings(List<GUIParameter> params) {
		SettingService.Instance.get().saveSettings(params, new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(Void ret) {
				GuiLog.info(I18N.message("settingssaved"), null);
			}
		});
	}
	
	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}