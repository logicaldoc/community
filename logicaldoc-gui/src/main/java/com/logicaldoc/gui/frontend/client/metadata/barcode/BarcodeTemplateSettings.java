package com.logicaldoc.gui.frontend.client.metadata.barcode;

import java.util.LinkedHashMap;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIBarcodeTemplate;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.Upload;
import com.logicaldoc.gui.frontend.client.services.BarcodeService;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.CloseClickEvent;
import com.smartgwt.client.widgets.events.CloseClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to upload / edit a barcode template
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
public class BarcodeTemplateSettings extends Window {

	private ValuesManager vm;

	private DynamicForm form;

	private IButton save;

	private BarcodeTemplatesPanel panel;

	private GUIBarcodeTemplate template;

	private Upload uploader;

	public BarcodeTemplateSettings(BarcodeTemplatesPanel panel, GUIBarcodeTemplate template) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("barcodetemplate"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setAutoSize(true);

		this.panel = panel;
		this.template = template;

		save = new IButton(I18N.message("save"));
		save.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				onSave();
			}
		});

		prepareForm();

		VLayout layout = new VLayout();
		layout.setMembersMargin(5);
		layout.setWidth100();

		layout.addMember(form);
		uploader = new Upload(save);
		layout.addMember(save);

		addItem(layout);

		// Clean the upload folder if the window is closed
		addCloseClickHandler(new CloseClickHandler() {
			@Override
			public void onCloseClick(CloseClickEvent event) {
				DocumentService.Instance.get().cleanUploadedFileFolder(new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						destroy();
					}
				});
			}
		});

		// Just to clean the upload folder
		DocumentService.Instance.get().cleanUploadedFileFolder(new AsyncCallback<Void>() {

			@Override
			public void onFailure(Throwable caught) {
				// Nothing to do
			}

			@Override
			public void onSuccess(Void result) {
				// Nothing to do
			}
		});
	}

	private void prepareForm() {
		form = new DynamicForm();
		form.setWidth100();
		form.setAlign(Alignment.LEFT);
		form.setColWidths("1px, 100%");
		vm = new ValuesManager();
		form.setValuesManager(vm);

		TextItem name = ItemFactory.newTextItem("name", template.getName());
		name.setRequired(true);
		name.setDisabled(template.getId() != 0L);

		StaticTextItem id = ItemFactory.newStaticTextItem("id", "" + template.getId());
		id.setVisible(template != null && template.getId() != 0L);

		SelectItem type = ItemFactory.newSelectItem("type");
		LinkedHashMap<String, String> opts = new LinkedHashMap<>();
		opts.put("zonal", I18N.message("zonal").toLowerCase());
		opts.put("positional", I18N.message("positional").toLowerCase());
		type.setValueMap(opts);
		type.setRequired(true);
		type.setDisabled(template.getId() != 0L || !Feature.enabled(Feature.ZONAL_BARCODE));
		if (template.getId() != 0L)
			type.setValue(template.isZonal() ? "zonal" : "positonal");
		if (!Feature.enabled(Feature.ZONAL_BARCODE))
			type.setValue("positonal");

		type.addChangedHandler(
				(ChangedEvent event) -> uploader.setVisible("zonal".equals(event.getValue().toString())));

		TextAreaItem description = ItemFactory.newTextAreaItem("description", "description", template.getDescription());
		description.setHeight(150);

		// The optional batch
		SpinnerItem batch = ItemFactory.newSpinnerItem("batch", template.getBatch());
		batch.setRequired(true);
		batch.setMin(1);
		batch.setStep(10);
		batch.setHintStyle("hint");

		// The image threshold
		SpinnerItem threshold = ItemFactory.newSpinnerItem("threshold", I18N.message("resolutionthreshold"),
				template.getThreshold());
		threshold.setRequired(true);
		threshold.setWrapTitle(false);
		threshold.setMin(50);
		threshold.setStep(100);
		threshold.setHint("pixels");

		// Resolution used to print the document
		SpinnerItem rendRes = ItemFactory.newSpinnerItem("rendres", I18N.message("ocrrendres"), template.getRendRes());
		rendRes.setRequired(true);
		rendRes.setWrapTitle(false);
		rendRes.setHint("dpi");
		rendRes.setMin(100);
		rendRes.setStep(100);

		if (Session.get().isDefaultTenant() && template.getId() != 0L)
			form.setItems(id, type, name, description, batch, threshold, rendRes);
		else
			form.setItems(id, type, name, description);
	}

	public void onSave() {
		if ("zonal".equals(vm.getValueAsString("type")) && template.getId() == 0L
				&& uploader.getUploadedFile() == null) {
			SC.warn(I18N.message("samplerequired"));
			return;
		}

		if (Boolean.FALSE.equals(vm.validate()))
			return;

		template.setName(vm.getValueAsString("name"));
		template.setDescription(vm.getValueAsString("description"));

		if (Feature.enabled(Feature.ZONAL_BARCODE))
			template.setZonal("zonal".equals(vm.getValueAsString("type")));
		else
			template.setZonal(false);

		if (Session.get().isDefaultTenant() && template.getId() != 0L) {
			template.setBatch(Integer.parseInt(vm.getValueAsString("batch")));
			template.setThreshold(Integer.parseInt(vm.getValueAsString("threshold")));
			template.setRendRes(Integer.parseInt(vm.getValueAsString("rendres")));
		}

		BarcodeService.Instance.get().save(template, new AsyncCallback<GUIBarcodeTemplate>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIBarcodeTemplate tmpl) {
				panel.setSelectedOcrTemplate(tmpl);
				destroy();
			}
		});
	}
}