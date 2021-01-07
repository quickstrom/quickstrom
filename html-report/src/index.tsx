import { h, render } from 'preact';
import App from './App';
import './reset.css';
import './report.css';

const root = document.getElementById('root');

render(<App />, root!);
