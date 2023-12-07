export default function FetchDataException(message = 'Failed to fetch data') {
  return new Error(message);
}
