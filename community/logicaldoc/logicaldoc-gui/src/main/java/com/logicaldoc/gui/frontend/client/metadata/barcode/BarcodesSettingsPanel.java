package com.logicaldoc.gui.frontend.client.metadata.barcode;

import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIBarcodeEngine;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.BarcodeService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * Panel showing the barcode processing infos.
 * 
 * @author Marco Mescheri - LogicalDOC
 * @since 6.1
 */
public class BarcodesSettingsPanel extends AdminPanel {

	private Layout engineTabPabel;

	private Layout schemeTabPanel;

	private GUIBarcodeEngine engine;

	private ValuesManager vm = new ValuesManager();

	public BarcodesSettingsPanel() {
		super("barcodes");
	}

	@Override
	public void onDraw() {
		BarcodeService.Instance.get().getInfo(new AsyncCallback<GUIBarcodeEngine>() {

			@Override
			public void onFailure(Throwable caught) {
				Log.serverError(caught);
			}

			@Override
			public void onSuccess(GUIBarcodeEngine engine) {
				BarcodesSettingsPanel.this.engine = engine;

				fillEngineTab();

				BarcodesSettingsPanel.this.tabs.addTab(fillSchemeTab());
				BarcodesSettingsPanel.this.tabs.addTab(fillQueueTab(100));
			}
		});
	}

	private Tab fillQueueTab(int maxValue) {
		Tab indexingQueueTab = new Tab(I18N.message("processingqueue"));
		indexingQueueTab.setPane(new BarcodeQueuePanel(maxValue));
		return indexingQueueTab;
	}

	private Tab fillSchemeTab() {
		Tab schemeTab = new Tab(I18N.message("patterns"));
		schemeTabPanel = new VLayout();
		schemeTabPanel.setWidth100();
		schemeTabPanel.setHeight100();
		schemeTab.setPane(new BarcodePatternsPanel());
		return schemeTab;
	}

	private void fillEngineTab() {
		engineTabPabel = new VLayout();
		engineTabPabel.setWidth100();
		engineTabPabel.setHeight100();

		DynamicForm form = new DynamicForm();
		form.setTitleOrientation(TitleOrientation.LEFT);
		form.setNumCols(2);
		form.setWrapItemTitles(false);
		form.setColWidths(1, "*");
		form.setValuesManager(vm);

		// Include Patters
		TextItem includePatters = ItemFactory.newTextItem("includePatters", "includepatters",
				this.engine.getIncludePatters());
		includePatters.setHint(I18N.message("separatedcomma"));
		includePatters.setHintStyle("hint");

		// Exclude Patters
		TextItem excludePatters = ItemFactory.newTextItem("excludePatters", "excludepatters",
				this.engine.getExcludePatters());
		excludePatters.setHint(I18N.message("separatedcomma"));
		excludePatters.setHintStyle("hint");

		// The optional batch
		IntegerItem batch = ItemFactory.newIntegerItem("batch", "batch", this.engine.getBatch());
		batch.setHintStyle("hint");

		// The image threshold
		IntegerItem resolutionThreshold = ItemFactory.newIntegerItem("resolutionthreshold",
				I18N.message("resolutionthreshold"), this.engine.getImageThreshold());
		resolutionThreshold.setRequired(true);
		resolutionThreshold.setWrapTitle(false);
		resolutionThreshold.setHint("pixels");

		HLayout buttons = new HLayout();

		IButton save = new IButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				@SuppressWarnings("unchecked")
				final Map<String, Object> values = vm.getValues();

				if (vm.validate()) {
					BarcodesSettingsPanel.this.engine.setIncludePatters((String) values.get("includePatters"));
					BarcodesSettingsPanel.this.engine.setExcludePatters((String) values.get("excludePatters"));
					String btch = vm.getValueAsString("batch");
					if (btch == null || "".equals(btch.trim()))
						BarcodesSettingsPanel.this.engine.setBatch(0);
					else
						BarcodesSettingsPanel.this.engine.setBatch(new Integer(btch));

					String threshold = vm.getValueAsString("resolutionthreshold");
					if (btch == null || "".equals(btch.trim()))
						BarcodesSettingsPanel.this.engine.setImageThreshold(0);
					else
						BarcodesSettingsPanel.this.engine.setImageThreshold(new Integer(threshold));

					BarcodeService.Instance.get().save(BarcodesSettingsPanel.this.engine, new AsyncCallback<Void>() {

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

		IButton rescheduleAll = new IButton();
		rescheduleAll.setAutoFit(true);
		rescheduleAll.setTitle(I18N.message("rescheduleallprocessing"));
		rescheduleAll.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				BarcodeService.Instance.get().rescheduleAll(new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(Void ret) {
						Log.info(I18N.message("docsrescheduledprocessing"), null);
					}
				});
			}
		});

		if (Session.get().isDefaultTenant())
			form.setItems(includePatters, excludePatters, batch, resolutionThreshold);
		else
			form.setItems(includePatters, excludePatters);
		buttons.setMembers(save, rescheduleAll);
		buttons.setMembersMargin(5);
		engineTabPabel.setMembers(form, buttons);
		engineTabPabel.setMembersMargin(15);
		engineTabPabel.setMargin(5);
		tab.setPane(engineTabPabel);
	}
}