import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

//Main.embed(document.getElementById('root'));
Main.fullscreen({
  backendUrl: "http://localhost:3000",
});

registerServiceWorker();
