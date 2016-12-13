/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package lipassistant;
import java.util.Iterator;
import jess.*;

/**
 *
 * @author ASUS X202E
 */
public class LipstickEngine {
    private Rete engine;
    private WorkingMemoryMarker marker;

    public LipstickEngine() throws JessException {
        // Create a Jess rule engine
        engine = new Rete();
        engine.reset();

        // Load the pricing rules
        engine.batch("lipstick.clp");

        // Load the catalog data into working memory
//        database = aDatabase;
//        engine.addAll(database.getCatalogItems());

       // Mark end of catalog data for later
       marker = engine.mark();
    }
    
    public Iterator run(Preference preference) throws JessException {
        engine.resetToMark(marker);
        engine.add(preference);
        engine.run();
        return engine.getObjects(new Filter.ByClass(Recommendation.class));
    }
}
