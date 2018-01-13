package com.logicaldoc.gui.frontend.client.settings;

import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;

/**
 * This panel shows the OCR settings.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class OCRSettingsPanel extends AdminPanel {

	private ValuesManager vm = new ValuesManager();

	private TextItem tesseract;

	private TextItem advancedOcrPath;

	public OCRSettingsPanel(GUIParameter[] settings) {
		super("ocr");

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

		IntegerItem resolutionThreshold = ItemFactory.newIntegerItem("ocr_resolution_threshold",
				I18N.message("resolutionthreshold"), null);
		resolutionThreshold.setRequired(true);
		resolutionThreshold.setWrapTitle(false);
		resolutionThreshold.setHint("pixels");

		IntegerItem textThreshold = ItemFactory.newIntegerItem("ocr_text_threshold", I18N.message("textthreshold"),
				null);
		textThreshold.setRequired(true);
		textThreshold.setWrapTitle(false);
		textThreshold.setHint("%");

		TextItem includes = ItemFactory.newTextItem("ocr_includes", "include", null);
		TextItem excludes = ItemFactory.newTextItem("ocr_excludes", "exclude", null);

		IntegerItem timeout = ItemFactory.newIntegerItem("ocr_timeout", I18N.message("timeout"), null);
		timeout.setRequired(true);
		timeout.setWrapTitle(false);
		timeout.setHint(I18N.message("seconds"));

		final RadioGroupItem engine = ItemFactory.newBooleanSelector("ocr_engine", "engine");
		engine.setRequired(true);
		engine.setValueMap("default", "advanced");
		engine.setValue(settings[6].getValue());
		engine.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				onEngineChanged(event.getValue().toString());
			}
		});

		tesseract = ItemFactory.newTextItem("command_tesseract", "Tesseract", null);
		tesseract.setWidth(350);

		advancedOcrPath = ItemFactory.newTextItem("advancedocr_path", "Advanced OCR path", null);
		advancedOcrPath.setWidth(350);

		IntegerItem ocrrendres = ItemFactory.newIntegerItem("ocr_rendres", I18N.message("ocrrendres"), null);
		ocrrendres.setRequired(true);
		ocrrendres.setWrapTitle(false);
		ocrrendres.setHint("dpi");

		IntegerItem barcoderendres = ItemFactory.newIntegerItem("ocr_rendres_barcode", I18N.message("barcoderendres"),
				null);
		barcoderendres.setRequired(true);
		barcoderendres.setWrapTitle(false);
		barcoderendres.setHint("dpi");

		IntegerItem batch = ItemFactory.newIntegerItem("ocr_batch", I18N.message("batch"), null);
		batch.setRequired(true);
		batch.setWrapTitle(false);
		batch.setHint("pages");

		for (GUIParameter param : settings) {
			if (param.getName().endsWith("ocr.enabled"))
				enabled.setValue(param.getValue().equals("true") ? "yes" : "no");
			else if (param.getName().endsWith("ocr.resolution.threshold"))
				resolutionThreshold.setValue(Integer.parseInt(param.getValue()));
			else if (param.getName().endsWith("ocr.text.threshold"))
				textThreshold.setValue(Integer.parseInt(param.getValue()));
			else if (param.getName().endsWith("ocr.timeout"))
				timeout.setValue(Integer.parseInt(param.getValue()));
			else if (param.getName().endsWith("ocr.rendres"))
				ocrrendres.setValue(Integer.parseInt(param.getValue()));
			else if (param.getName().endsWith("ocr.rendres.barcode"))
				barcoderendres.setValue(Integer.parseInt(param.getValue()));
			else if (param.getName().endsWith("ocr.batch"))
				batch.setValue(Integer.parseInt(param.getValue()));
			else if (param.getName().endsWith(".ocr.includes"))
				includes.setValue(param.getValue());
			else if (param.getName().endsWith(".ocr.excludes"))
				excludes.setValue(param.getValue());
			else if (param.getName().endsWith("ocr.engine"))
				engine.setValue(param.getValue());
			else if (param.getName().endsWith("command.tesseract"))
				tesseract.setValue(param.getValue());
			else if (param.getName().endsWith("advancedocr.path"))
				advancedOcrPath.setValue(param.getValue());
		}

		if (Session.get().isDefaultTenant())
			form.setItems(enabled, timeout, includes, excludes, textThreshold, resolutionThreshold, ocrrendres,
					barcoderendres, batch, engine, tesseract, advancedOcrPath);
		else
			form.setItems(enabled, includes, excludes, textThreshold, resolutionThreshold);

		IButton save = new IButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			@SuppressWarnings("unchecked")
			public void onClick(ClickEvent event) {
				Map<String, Object> values = (Map<String, Object>) vm.getValues();

				if (vm.validate()) {
					GUIParameter[] params = new GUIParameter[12];

					params[1] = new GUIParameter(Session.get().getTenantName() + ".ocr.includes", (String) values
							.get("ocr_includes"));
					params[2] = new GUIParameter(Session.get().getTenantName() + ".ocr.excludes", (String) values
							.get("ocr_excludes"));
					if (values.get("ocr_text_threshold") instanceof Integer)
						params[3] = new GUIParameter(Session.get().getTenantName() + ".ocr.text.threshold",
								((Integer) values.get("ocr_text_threshold")).toString());
					else
						params[3] = new GUIParameter(Session.get().getTenantName() + ".ocr.text.threshold",
								(String) values.get("ocr_text_threshold"));

					if (values.get("ocr_resolution_threshold") instanceof Integer)
						params[4] = new GUIParameter(Session.get().getTenantName() + ".ocr.resolution.threshold",
								((Integer) values.get("ocr_resolution_threshold")).toString());
					else
						params[4] = new GUIParameter(Session.get().getTenantName() + ".ocr.resolution.threshold",
								(String) values.get("ocr_resolution_threshold"));

					if (Session.get().isDefaultTenant()) {
						if (values.get("ocr_timeout") instanceof Integer)
							params[5] = new GUIParameter("ocr.timeout", ((Integer) values.get("ocr_timeout"))
									.toString());
						else
							params[5] = new GUIParameter("ocr.timeout", (String) values.get("ocr_timeout"));

						if (values.get("ocr_rendres") instanceof Integer)
							params[6] = new GUIParameter("ocr.rendres", ((Integer) values.get("ocr_rendres"))
									.toString());
						else
							params[6] = new GUIParameter("ocr.rendres", (String) values.get("ocr_rendres"));

						if (values.get("ocr_rendres_barcode") instanceof Integer)
							params[7] = new GUIParameter("ocr.rendres.barcode", ((Integer) values
									.get("ocr_rendres_barcode")).toString());
						else
							params[7] = new GUIParameter("ocr.rendres.barcode", (String) values
									.get("ocr_rendres_barcode"));

						if (values.get("ocr_batch") instanceof Integer)
							params[11] = new GUIParameter("ocr.batch", ((Integer) values.get("ocr_batch")).toString());
						else
							params[11] = new GUIParameter("ocr.batch", (String) values.get("ocr_batch"));

						params[8] = new GUIParameter("ocr.engine", (String) values.get("ocr_engine"));
						params[9] = new GUIParameter("command.tesseract", (String) values.get("command_tesseract"));
						params[10] = new GUIParameter("advancedocr.path", (String) values.get("advancedocr_path"));
					}

					SettingService.Instance.get().saveSettings(params, new AsyncCallback<Void>() {

						@Override
						public void onFailure(Throwable caught) {
							Log.serverError(caught);
						}

						@Override
						public void onSuccess(Void ret) {
							Log.info(I18N.message("settingssaved"), null);
						}
					});
				}
			}
		});

		onEngineChanged(engine.getValueAsString());

		body.setMembers(form);
		addMember(save);
	}

	private void onEngineChanged(String engine) {
		if ("default".equals(engine)) {
			tesseract.show();
			advancedOcrPath.hide();
		} else {
			tesseract.hide();
			advancedOcrPath.show();
		}
	}
}