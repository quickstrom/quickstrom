import { h, render } from 'preact';
import App, { Report, Result } from '../src/App';
import '../src/reset.css';
import '../src/report.css';
import report from "./report.json";

const root = document.getElementById('root');

render(<App report={report as Report<Result>} />, root!);