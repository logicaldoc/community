package com.logicaldoc.core.filler;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider;
import org.springframework.core.type.filter.AnnotationTypeFilter;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.automation.Automation;
import com.logicaldoc.core.automation.AutomationException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentManager;
import com.logicaldoc.core.document.IndexingStatus;
import com.logicaldoc.core.history.History;
import com.logicaldoc.core.metadata.ExtensibleObject;
import com.logicaldoc.core.parser.ParsingException;
import com.logicaldoc.core.runtime.Aspect;
import com.logicaldoc.core.runtime.FeatureDisabledException;
import com.logicaldoc.core.runtime.RunLevel;
import com.logicaldoc.core.searchengine.Hit;
import com.logicaldoc.core.searchengine.SearchEngine;
import com.logicaldoc.core.searchengine.SearchException;
import com.logicaldoc.util.Value;

import jakarta.persistence.Column;
import jakarta.persistence.DiscriminatorColumn;
import jakarta.persistence.DiscriminatorType;
import jakarta.persistence.DiscriminatorValue;
import jakarta.persistence.Entity;
import jakarta.persistence.Inheritance;
import jakarta.persistence.InheritanceType;
import jakarta.persistence.Table;

/**
 * A Filler implements its own logic to auto-fill the metadata of an
 * {@link ExtensibleObject}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.3
 */
@Entity
@Table(name = "ld_filler")
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
@DiscriminatorColumn(name = "ld_type", discriminatorType = DiscriminatorType.STRING)
public abstract class Filler extends PersistentObject {

    private static final long serialVersionUID = 1L;

    private static final Logger log = LoggerFactory.getLogger(Filler.class);

    @Column(name = "ld_name", length = 255, nullable = false)
    protected String name;

    @Column(name = "ld_label", length = 255, nullable = true)
    protected String label;

    @Column(name = "ld_description", nullable = true)
    protected String description;

    /**
     * Specifies if the document must be re-filled at checkin
     */
    @Column(name = "ld_checkin", nullable = false)
    protected boolean checkin = false;

    /**
     * Specifies if already filled properties must be overwritten
     */
    @Column(name = "ld_overwrite", nullable = false)
    protected boolean overwrite = false;

    /**
     * Optional automation procedure invoked after the filling
     */
    @Column(name = "ld_automation", nullable = true)
    protected String automation;

    private static String EXPLICATION_TOP = """
            <html>
            <header>
              <style>
                body {
                  font-family: arial;
                  font-size: 0.9rem;
                }
                
                th {
                 white-space: nowrap; 
                 text-align: left; 
                 background-color: WhiteSmoke;
                 vertical-align: middle; 
                 height: 1px;
                }
                
                th.title {
                 background-color: RoyalBlue;
                 color: White;
                 font-weight: bold;
                }
                
                .accordion {
                  background-color: #eee;
                  color: #444;
                  cursor: pointer;
                  padding: 8px;
                  width: 100\u0025;
                  border: none;
                  text-align: left;
                  outline: none;
                  font-size: 15px;
                  transition: 0.4s;
                }
                
                .active, .accordion:hover {
                  background-color: #ccc; 
                }
                
                .panel {
                  padding: 0 18px;
                  display: none;
                  background-color: white;
                  overflow: hidden;
                }    
                
                    
          /* Thumbnail */
          .thumb {
            max-width: 300px;
            cursor: pointer;
          }

          /* Lightbox overlay (hidden by default) */
          .lightbox {
            position: fixed;
            inset: 0;
            background: rgba(0,0,0,0.8);
            display: none;
            align-items: center;
            justify-content: center;
            z-index: 999;
          }

          /* Show when targeted */
          .lightbox:target {
            display: flex;
          }

          .lightbox img {
            max-width: 90vw;
            max-height: 90vh;
            box-shadow: 0 0 20px #000;
          }

          /* Click anywhere to close */
          .lightbox-close {
            position: fixed;
            inset: 0;
          }
                            
              </style>
            </header>
            <body>
        """;

    private static String EXPLICATION_BOTTOM = """        
                <script>
                    var acc = document.getElementsByClassName("accordion");
                    var i;
                    
                    for (i = 0; i < acc.length; i++) {
                      acc[i].addEventListener("click", function() {
                        this.classList.toggle("active");
                        var panel = this.nextElementSibling;
                        if (panel.style.display === "block") {
                          panel.style.display = "none";
                        } else {
                          panel.style.display = "block";
                        }
                      });
                    }
                </script>       
            </body>
        </html>
        """;

    protected static String FILLER_EXPLICATION_TOP = """
        <table align='left' border='1' width='100%%'>
            <thead>
               <tr>
                 <th class='title'>%s &nbsp;&nbsp; <small><em>%s</em></small></th>
               </tr>
            </thead>
            <tbody>
            <tr><td>
        """;

    protected static String FILLER_EXPLICATION_BOTTOM = "</td></tr></tbody></table>";

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public boolean isCheckin() {
        return checkin;
    }

    public void setCheckin(boolean checkin) {
        this.checkin = checkin;
    }

    public boolean isOverwrite() {
        return overwrite;
    }

    public void setOverwrite(boolean overwrite) {
        this.overwrite = overwrite;
    }

    public String getAutomation() {
        return automation;
    }

    public void setAutomation(String automation) {
        this.automation = automation;
    }

    protected String getExplicationSubtitle() {
        return "";
    }

    /**
     * Factory method for instantiating a new filler
     * 
     * @param type Type of filler, matches the discriminator value
     * 
     * @return The new filler
     * 
     * @throws IllegalArgumentException No implementation found for the given
     *         type
     */
    public static Filler newFiller(String type) {
        ClassPathScanningCandidateComponentProvider scanner = new ClassPathScanningCandidateComponentProvider(false);
        scanner.addIncludeFilter(new AnnotationTypeFilter(DiscriminatorValue.class));
        for (BeanDefinition bd : scanner.findCandidateComponents("com.logicaldoc")) {
            try {
                Class<?> beanClass = Class.forName(bd.getBeanClassName());
                if (Filler.class.isAssignableFrom(beanClass)
                        && type.equals(beanClass.getAnnotation(DiscriminatorValue.class).value()))
                    return (Filler) beanClass.getDeclaredConstructor().newInstance();
            } catch (ClassNotFoundException | InstantiationException | IllegalAccessException | IllegalArgumentException
                    | InvocationTargetException | NoSuchMethodException | SecurityException e) {
                log.warn(e.getMessage(), e);
            }
        }

        throw new IllegalArgumentException("Cannot find any filler of type %s".formatted(type));
    }

    /**
     * Place where implementations put the logic to fill a {@link Document}
     * 
     * @param document the instance to fill
     * @param content the content of the object, if not specified it will be
     *        taken from the transaction's file.
     * @param transaction the current transaction
     * @param dictionary Dictionary of the execution pipeline
     * @param explication optional buffer to receive the explication of the
     *        processing
     * 
     * @return String representation of the filled content
     * 
     * @throws PersistenceException Error in the data layer
     * @throws IOException I/O error
     * @throws FeatureDisabledException An involved feature is disabled
     * @throws SearchException Error in case of search
     * @throws AutomationException Error in the automation (if the filler makes
     *         use of automation)
     */
    protected abstract String fillDocument(Document document, String content, History transaction,
            Map<String, Object> dictionary, StringBuilder explication)
            throws PersistenceException, IOException, FeatureDisabledException, SearchException, AutomationException;

    /**
     * Fills a {@link Document} and then records a FILLED event.
     * 
     * @param document the instance to fill
     * @param content the content of the object, if not specified it will be
     *        taken from the transaction's file.
     * @param transaction the current transaction
     * @param dictionary Dictionary of the execution pipeline
     * @param explication optional buffer to receive the explication of the
     *        processing
     * 
     * @return String representation of the filled content
     * 
     * @throws PersistenceException Error in the data layer
     * @throws IOException I/O error
     * @throws FeatureDisabledException An involved feature is disabled
     * @throws SearchException Error in case of search
     * @throws AutomationException Error in the automation (if the filler makes
     *         use of automation)
     */
    public final String fill(Document document, String content, History transaction, Map<String, Object> dictionary,
            StringBuilder explication)
            throws PersistenceException, IOException, FeatureDisabledException, SearchException, AutomationException {

        if (document.getFormId() != null)
            throw new PersistenceException("Cannot fill a form");
        if (!RunLevel.current().aspectEnabled(Aspect.AUTOFILL))
            return null;
        
        if (explication != null) {
            explication.append(EXPLICATION_TOP);
            explication.append(
                    FILLER_EXPLICATION_TOP.formatted(this.getClass().getSimpleName(), getExplicationSubtitle()));
        }

        if(dictionary==null)
            dictionary = new HashMap<>();
        
        String value = fillDocument(document, content, transaction, dictionary, explication);

        // In any case after filling we must update the filled flag
        if (!document.isFilled()) {
            document.setFilled(true);
            if (log.isDebugEnabled())
                log.debug("Document {} marked as filled", document);
        }

        if (explication != null) {
            explication.append(FILLER_EXPLICATION_BOTTOM);
            explication.append(EXPLICATION_BOTTOM);
        }

        // Record the filled event just in case of concrete modification
        if (transaction != null && document.isModified()) {
            DocumentHistory fillHistory = new DocumentHistory(transaction);
            fillHistory.setEvent(DocumentEvent.FILLED);
            fillHistory.setComment("%s > %s".formatted(this.getClass().getSimpleName(), value));
            DocumentDAO.get().saveDocumentHistory(document, fillHistory);
        }

        return value;
    }

    /**
     * Fills a {@link Document} using the body text as input
     * 
     * @param document the document to fill
     * @param transaction the current transaction
     * @param dictionary Dictionary of the execution pipeline
     * @param explication optional buffer to receive the explication of the
     *        processing
     * 
     * @return String representation of the filled content
     * 
     * @throws PersistenceException Error in the data layer
     * @throws IOException I/O error
     * @throws ParsingException The document cannot be parsed and no texts were
     *         extracted
     * @throws FeatureDisabledException An involved feature is disabled
     * @throws SearchException Error in case of search
     * @throws AutomationException Error in the automation execution(if any
     *         automation script has been provided)
     */
    public final String fill(Document document, DocumentHistory transaction, Map<String, Object> dictionary,
            StringBuilder explication) throws PersistenceException, IOException, FeatureDisabledException,
            ParsingException, SearchException, AutomationException {

        if (document.getIndexingStatus().equals(IndexingStatus.TO_INDEX)) {
            DocumentManager.get().index(document.getId(), null, new DocumentHistory(transaction));
            document.setIndexingStatus(IndexingStatus.INDEXED);
        }

        Hit hit = SearchEngine.get().getHit(document.getId());
        String extractedContent = hit != null ? hit.getContent() : "";
        if (StringUtils.isBlank(extractedContent))
            throw new ParsingException("Cannot extract any content from document %s".formatted(document));

        if (log.isDebugEnabled())
            log.debug("Filling documnent {} using text {}", document, StringUtils.abbreviate(extractedContent, 150));

        String result = fill(document, extractedContent, transaction, dictionary, explication);

        Value<String> extractedValue = new Value<>();
        if (StringUtils.isNotEmpty(automation)) {
            Automation script = new Automation("Filler-%s".formatted(name), null, getTenantId());
            script.evaluate(automation, Map.of("filler", this, "document", document, "transaction", transaction,
                    "fillerDictionary", dictionary, "value", extractedValue));
        }

        return StringUtils.defaultIfEmpty(extractedValue.getValue(), result);
    }

    protected boolean mustOverwrite(History transaction) {
        if (transaction.getOverwrite() != null)
            return transaction.getOverwrite().booleanValue();
        else
            return isOverwrite();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((label == null) ? 0 : label.hashCode());
        result = prime * result + ((name == null) ? 0 : name.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        Filler other = (Filler) obj;
        if (label == null) {
            if (other.label != null)
                return false;
        } else if (!label.equals(other.label))
            return false;
        if (name == null)
            return other.name == null;
        else
            return name.equals(other.name);
    }

    @Override
    public String toString() {
        return "%s(%d)".formatted(name, id);
    }
}