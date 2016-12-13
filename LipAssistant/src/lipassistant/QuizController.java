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
public class QuizController  {
    
    private static QuizFrame quizFrame;
    private static LipstickEngine engine;
    
    public QuizController() throws JessException {
        quizFrame = new QuizFrame(this);
        engine = new LipstickEngine();
    }
    
    public void start() {
        quizFrame.setVisible(true);
    }
    
    public Recommendation getRecommendation(Preference preference) throws JessException {
        Iterator i = engine.run(preference);
        return (Recommendation)i.next();
    }
    
}
